import re

from bs4 import BeautifulSoup

from backend.importer.agnostic import UNITS, Importer
from backend.main import (
    Ingredient,
    IngredientGroup,
    InstructionGroup,
    Receipe,
    Servings,
)


class BiancaZapatkaImporter(Importer):
    @staticmethod
    def _parse_ingredient_amount(tag) -> float | None:
        if tag is not None:
            text = re.sub(r"\s*½", ".5", tag.text)
            return float(text)
        else:
            return None

    @staticmethod
    def _parse_ingredient(tag) -> Ingredient:

        notes_tag = tag.find("span", class_="wprm-receipe-ingredient-notes")
        unit_tag = tag.find("span", class_="wprm-recipe-ingredient-unit")
        amount_tag = tag.find("span", class_="wprm-recipe-ingredient-amount")

        unit = ""
        if unit_tag:
            for u in UNITS:
                if unit_tag.text == u.symbol or unit_tag.text in u.aliases:
                    unit = u.id
                    break

        ing = Ingredient(
            amount=BiancaZapatkaImporter._parse_ingredient_amount(amount_tag),
            unit=unit,
            name=tag.find("span", class_="wprm-recipe-ingredient-name").text,
            comment=notes_tag.text if notes_tag and notes_tag.text else None,
        )
        return ing

    def parse_receipe(self, html: str) -> Receipe:
        soup = BeautifulSoup(html, features="lxml")
        receipe_container = soup.find(
            "div",
            attrs={"class": "wprm-recipe-template-biancazapatka-new-de-container"},
        )

        title = receipe_container.find("h2", attrs={"class": "wprm-recipe-name"}).text
        cuisine = receipe_container.find("span", class_="wprm-recipe-cuisine").text
        servings_amount = int(
            receipe_container.find("span", class_="wprm-recipe-servings").text
        )
        servings_unit = receipe_container.find(
            "span", class_="wprm-recipe-servings-unit"
        ).text

        ingredients_groups = [
            IngredientGroup(
                name=group.find("h4", class_="wprm-recipe-group-name").text,
                ingredients=[
                    self._parse_ingredient(item)
                    for item in group.find_all("li", class_="wprm-recipe-ingredient")
                ],
            )
            for group in receipe_container.find_all(
                "div", class_="wprm-recipe-ingredient-group"
            )
        ]

        instruction_groups = [
            InstructionGroup(
                group.find("h4").text,
                [
                    tag.text
                    for tag in group.find_all(
                        "div", class_="wprm-recipe-instruction-text"
                    )
                ],
            )
            for group in receipe_container.find_all(
                "div", class_="wprm-recipe-instruction-group"
            )
            if group.find("li")
        ]

        return Receipe(
            id=-1,
            title=title,
            ingredients=ingredients_groups,
            instructions=instruction_groups,
            servings=Servings(amount=servings_amount, unit=servings_unit),
            tags=["importiert", "Bianca Zapatka", "vegan", cuisine],
        )
