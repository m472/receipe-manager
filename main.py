from __future__ import annotations
import json
from dataclasses import dataclass
from pathlib import Path
from copy import deepcopy
from typing import TypeVar, Type

from flask import Flask, redirect, render_template, request, send_from_directory
from werkzeug.wrappers.response import Response
from serde.json import from_json
from serde import from_dict

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
class Receipe:
    id: int
    title: str
    ingredients: list[IngredientGroup]

    def get_image_ids(self) -> list[int]:
        return [
            int(f.name.split("_")[1].split(".")[0])
            for f in RECEIPE_IMG_DIR.glob(f"{self.id}_*.jpg")
        ]

    @classmethod
    def load(cls: Type[_T], path: Path) -> _T:
        return from_json(cls, path.read_text())


@dataclass
class Cookbook:
    receipes: dict[int, Receipe]
    units: dict[str, Unit]

    @staticmethod
    def load() -> Cookbook:
        receipes = [Receipe.load(f) for f in RECEIPE_DIR.iterdir() if f.is_file()]
        units = [from_dict(Unit, f) for f in json.loads(UNITS_FILE.read_text())]
        return Cookbook(
            receipes={r.id: r for r in receipes}, units={u.id: u for u in units}
        )


COOKBOOK = Cookbook.load()


@app.route("/")
def home() -> str:
    return render_template("home.html", cookbook=Cookbook.load())


def try_parse(_type: Type[_TNumber], arg: str) -> _TNumber | None:
    try:
        return _type(arg)
    except ValueError:
        return None


@app.route("/receipe")
def display_receipe() -> str:
    _id = int(request.args["id"])
    receipe = deepcopy(COOKBOOK.receipes[_id])

    if "factor" in request.args:
        for t in [int, float]:
            factor = try_parse(t, request.args["factor"])
            if factor is not None:
                break
        else:
            factor = 1
    else:
        factor = 1

    for ig in receipe.ingredients:
        for i in ig.ingredients:
            if isinstance(i.amount, (int, float)):
                i.amount *= factor

    print(f"{receipe.get_image_ids() = }")

    return render_template(
        "receipe.html",
        receipe=receipe,
        image_ids=receipe.get_image_ids(),
        units=COOKBOOK.units,
    )


@app.route("/receipe/image")
def get_image() -> Response:
    receipe_id = int(request.args["receipe_id"])
    image_id = int(request.args["image_id"])
    return send_from_directory(RECEIPE_IMG_DIR, f"{receipe_id}_{image_id}.jpg")


@app.route("/receipe/edit")
def edit_receipe() -> str:
    _id = int(request.args["id"])
    receipe = COOKBOOK.receipes[_id]

    return render_template(
        "edit_receipe.html",
        receipe=receipe,
        image_ids=receipe.get_image_ids(),
        units=COOKBOOK.units,
    )


@app.route("/receipe/update")
def update_receipe() -> Response:
    return redirect("edit_receipe")


if __name__ == "__main__":
    app.run()
