import re
from abc import ABC, abstractmethod
from typing import Callable

from backend.datamodel import Ingredient, Receipe, Unit

UNITS: list[Unit] = [
    Unit("", "", 1),
    Unit("el", "EL", 1),
    Unit("tl", "TL", 1),
    Unit("stk", "Stück", 1, ["Stk."]),
    Unit("gr", "g", 1, ["gr", "gramm"]),
    Unit("kg", "kg", 1),
    Unit("bund", "Bund", 1),
]


def parse_ingredient(ingredient: str) -> Ingredient:
    amount, remainder = parse_ingredient_amount(ingredient)
    unit, remainder = parse_ingredient_unit(remainder)

    if m := re.match(r"(.*)\s\((.*)\)", remainder):
        name = m.group(1)
        comment = m.group(2)
    else:
        name = remainder
        comment = None

    return Ingredient(amount, unit.id, name, comment)


def parse_fraction(inp: str) -> float:
    nom, den = inp.split("/")
    return int(nom) / int(den)


def parse_ingredient_amount(inp: str) -> tuple[float | None, str]:
    head, tail = inp.split(maxsplit=1)

    parsers: list[Callable[[str], float]] = [float, parse_fraction]
    for parser in parsers:
        try:
            return (parser(head), tail)
        except Exception:
            pass

    return (None, inp)


def parse_ingredient_unit(inp: str) -> tuple[Unit, str]:
    try:
        head, tail = map(str.strip, inp.split(" ", 1))

        for unit in UNITS:
            if head == unit.symbol or head in unit.aliases:
                return (unit, tail)
    except Exception:
        pass

    return (Unit("", "", 1), inp)


class Importer(ABC):
    @abstractmethod
    def parse_receipe(self, html: str) -> Receipe:
        ...
