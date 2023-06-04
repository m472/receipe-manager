from __future__ import annotations

import json
from copy import deepcopy
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, Type, TypeVar

from flask import Flask, jsonify, render_template, request, send_from_directory
from serde import SerdeError, from_dict
from serde.json import from_json, to_json

if TYPE_CHECKING:
    from werkzeug.wrappers.response import Response

_T = TypeVar("_T")

app = Flask(__name__)

DATA_DIR = Path("data")
RECEIPE_DIR = DATA_DIR / "receipes"
RECEIPE_IMG_DIR = RECEIPE_DIR / "images"
UNITS_FILE = DATA_DIR / "units.json"


@dataclass
class Unit:
    id: str
    symbol: str
    si_conversion_factor: float


@dataclass
class Ingredient:
    amount: float | int | None
    unit: str
    name: str
    comment: str | None


@dataclass
class IngredientGroup:
    name: str
    ingredients: list[Ingredient]


@dataclass
class InstructionGroup:
    name: str
    steps: list[str]


@dataclass
class Servings:
    amount: int
    unit: str


@dataclass
class Receipe:
    id: int
    title: str
    ingredients: list[IngredientGroup]
    instructions: list[InstructionGroup]
    servings: Servings
    image_ids: list[int] = field(default_factory=list)

    def get_image_ids(self) -> list[int]:
        return [
            int(f.name.split("_")[1].split(".")[0])
            for f in RECEIPE_IMG_DIR.glob(f"{self.id}_*.jpg")
        ]

    def multiply_by(self, factor: int | float) -> Receipe:
        res = deepcopy(self)

        for ig in res.ingredients:
            for i in ig.ingredients:
                if isinstance(i.amount, (int, float)):
                    i.amount *= factor

        return res

    @classmethod
    def load(cls: Type[_T], path: Path) -> _T:
        return from_json(cls, path.read_text())

    def save(self) -> None:
        COOKBOOK.receipes[self.id] = self
        (RECEIPE_DIR / f"{self.id}.json").write_text(to_json(self))


@dataclass
class Cookbook:
    receipes: dict[int, Receipe]
    units: dict[str, Unit]

    @staticmethod
    def load() -> Cookbook:
        receipes = [
            Receipe.load(f)
            for f in RECEIPE_DIR.iterdir()
            if f.is_file() and f.name.endswith(".json")
        ]
        units = [from_dict(Unit, f) for f in json.loads(UNITS_FILE.read_text())]

        return Cookbook(
            receipes={r.id: r for r in receipes},
            units={u.id: u for u in units},
        )


COOKBOOK = Cookbook.load()


@app.route("/")
@app.route("/<path:path>")
def home(path: str = "") -> str:
    return render_template("home.html", path=path)


@app.route("/receipe/list")
def receipe_list() -> Response:
    return jsonify(
        [
            {
                "id": id,
                "title": receipe.title,
                "image_ids": receipe.image_ids,
            }
            for id, receipe in COOKBOOK.receipes.items()
        ]
    )


@app.route("/receipe")
def json_receipe() -> Response:
    _id = int(request.args["id"])

    receipe = COOKBOOK.receipes[_id]
    receipe.image_ids = receipe.get_image_ids()
    return jsonify(asdict(receipe) | {"units": COOKBOOK.units})


@app.route("/receipe/update", methods=["POST"])
def json_receipe_update() -> tuple[Response, int]:
    try:
        id = int(request.args["id"])
        receipe = from_json(Receipe, request.data)
        receipe.save()
        assert receipe.id == id

    except ValueError:
        status_code = 400
        status = "Error parsing url request parameter 'id'"

    except SerdeError as se:
        status_code = 400
        status = f"Parsing Error: {se}"

    except AssertionError:
        status_code = 400
        status = "Assertion Error"

    else:
        status_code = 200
        status = "success"

    return (jsonify({"status": status}), status_code)


@app.route("/receipe/create", methods=["POST"])
def json_receipe_create() -> tuple[Response, int]:
    _id = max(COOKBOOK.receipes) + 1
    new_receipe = Receipe(_id, "", [], Servings(4, "Portionen"))
    new_receipe.save()

    return jsonify(asdict(new_receipe) | {"units": COOKBOOK.units}), 200


@app.route("/receipe/delete", methods=["POST"])
def json_receipe_delete() -> tuple[str, int]:
    try:
        _id = int(request.args["id"])

        # delete all images associated with the receipe
        for img_id in COOKBOOK.receipes[_id].get_image_ids():
            (RECEIPE_IMG_DIR / f"{_id}_{img_id}.jpg").unlink()

        # delete receipe
        (RECEIPE_DIR / f"{_id}.json").unlink()

        # remove from cache
        del COOKBOOK.receipes[_id]

    except ValueError:
        status_code = 400
        status = "Error parsing url request parameter 'id'"

    else:
        status_code = 200
        status = "deletion successful"

    return status, status_code


@app.route("/receipe/image")
def get_image() -> Response:
    receipe_id = int(request.args["receipe_id"])
    image_id = int(request.args["image_id"])
    return send_from_directory(RECEIPE_IMG_DIR, f"{receipe_id}_{image_id}.jpg")

@app.route("/receipe/image/upload", methods=["PUT"])
def upload_image() -> Response:
    receipe_id = int(request.args["receipe_id"])
    image_id = int(request.args["image_id"])
    (RECEIPE_IMG_DIR / f"{receipe_id}_{image_id}.jpg").write_bytes(request.data)
    return jsonify({"status": "success"})

@app.route("/receipe/image/delete", methods=["POST"])
def delete_image() -> Response:
    receipe_id = int(request.args["receipe_id"])
    image_id = int(request.args["image_id"])
    (RECEIPE_IMG_DIR / f"{receipe_id}_{image_id}.jpg").unlink()
    return jsonify({"status": "success"})


if __name__ == "__main__":
    app.run()
