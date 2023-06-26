from __future__ import annotations

import json
from copy import deepcopy
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING

from serde import SerdeError, from_dict
from serde.json import from_json, to_json

if TYPE_CHECKING:
    from typing_extensions import Self

DATA_DIR = Path("data")
RECEIPE_DIR = DATA_DIR / "receipes"
RECEIPE_IMG_DIR = RECEIPE_DIR / "images"
UNITS_FILE = DATA_DIR / "units.json"


@dataclass
class Unit:
    id: str
    symbol: str
    si_conversion_factor: float
    aliases: list[str] = field(default_factory=list)


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
    tags: list[str] = field(default_factory=list)

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
    def load(cls, path: Path) -> Self:
        text = path.read_text()
        print(cls)
        return from_json(cls, text)

    def save(self) -> None:
        COOKBOOK.receipes[self.id] = self
        (RECEIPE_DIR / f"{self.id}.json").write_text(to_json(self))


@dataclass
class Cookbook:
    receipes: dict[int, Receipe]
    units: dict[str, Unit]

    @staticmethod
    def load() -> Cookbook:
        receipes = []
        for f in RECEIPE_DIR.iterdir():
            if f.is_file() and f.name.endswith(".json"):
                try:
                    receipes.append(Receipe.load(f))
                except SerdeError as ex:
                    print(f"could not load receipe {f}")
                    print(ex)

        units = [from_dict(Unit, f) for f in json.loads(UNITS_FILE.read_text())]

        return Cookbook(
            receipes={r.id: r for r in receipes},
            units={u.id: u for u in units},
        )


COOKBOOK = Cookbook.load()
