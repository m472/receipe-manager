from bs4.element import Tag
import requests
from bs4 import BeautifulSoup
from dataclasses import dataclass
from enum import Enum
import re


class UnitType(Enum):
    VOLUME = "VOLUME"
    MASS = "MASS"
    COUNT = "COUNT"


@dataclass
class Unit:
    symbol: str
    kind: UnitType


@dataclass
class Ingredient:
    amount: float | int | None
    amount_max: float | int | None
    unit: Unit
    name: str
    comment: str | None


class IngredientGroup:
    name: str | None
    ingredients: list[Ingredient]


@dataclass
class Receipe:
    title: str
    amount: int
    unit: str
    instructions: list[str]
    ingredients: list[IngredientGroup]


class EatThisImporter:
    def _parse_ingredient(self, ingredient: Tag) -> Ingredient:
        m = re.match(
            r"((?P<amount>[0-9\.]+))\s*(?P<max_amount>-\s*\d+)?\s*(?P<unit>\w+)?)?\s*(?P<name>.*)",
            ingredient.text,
        )
        print(m)
        if m:
            if m["amount"]:
                amount = int(m["amount"])
            else:
                amount = None

            return Ingredient(
                amount=amount,
                amount_max=m["max_amount"],
                unit=unit,
                name=m["name"],
                comment=None,
            )

        else:
            raise Exception()

    def parse_receipe(self, html: str) -> Receipe:
        soup = BeautifulSoup(html)

        receipe_container = soup.find("div", attrs={"class": "entry-content"})
        title = soup.title.text.split("•")[0].strip()

        for ingredient_group in receipe_container.find_all('ul'):
            

        return Receipe(
            title,
        )
