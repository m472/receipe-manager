from __future__ import annotations
import json
from dataclasses import asdict, dataclass
from pathlib import Path
from copy import deepcopy
from typing import TypeVar, Type, cast

from flask import (
    Flask,
    jsonify,
    redirect,
    render_template,
    request,
    send_from_directory,
)
from werkzeug.wrappers.response import Response
from serde.json import from_json, to_json
from serde import SerdeError, from_dict

_T = TypeVar("_T")
_TNumber = TypeVar("_TNumber", int, float)

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
class Servings:
    amount: int
    unit: str = "Portionen"


@dataclass
class Receipe:
    id: int
    title: str
    ingredients: list[IngredientGroup]
    servings: Servings
    image_ids: list[int] | None = None

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
        units = [
            cast(Unit, from_dict(Unit, f)) for f in json.loads(UNITS_FILE.read_text())
        ]
        return Cookbook(
            receipes={r.id: r for r in receipes}, units={u.id: u for u in units}
        )


COOKBOOK = Cookbook.load()


@app.route("/")
def home() -> str:
    return render_template("home.html")


@app.route("/receipe/json")
def json_receipe() -> Response:
    _id = int(request.args["id"])

    receipe = COOKBOOK.receipes[_id]
    receipe.image_ids = receipe.get_image_ids()
    return jsonify(asdict(receipe) | {"units": COOKBOOK.units})


@app.route("/receipe/json/update", methods=["POST"])
def json_receipe_update() -> tuple[Response, int]:
    try:
        id = int(request.args["id"])
        receipe = from_json(Receipe, request.data)
        COOKBOOK.receipes[id] = receipe
        (RECEIPE_DIR / f"{id}.json").write_text(to_json(receipe))

    except SerdeError as se:
        status_code = 400
        status = f"Parsing Error: {se}"
        print(request.data)

    else:
        status_code = 200
        status = "success"

    return (jsonify({"status": status}), status_code)


@app.route("/receipe/image")
def get_image() -> Response:
    receipe_id = int(request.args["receipe_id"])
    image_id = int(request.args["image_id"])
    return send_from_directory(RECEIPE_IMG_DIR, f"{receipe_id}_{image_id}.jpg")


if __name__ == "__main__":
    app.run()
