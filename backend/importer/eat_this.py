import re

from bs4 import BeautifulSoup

from backend.datamodel import IngredientGroup, Receipe, Servings
from backend.importer.agnostic import Importer, parse_ingredient


class EatThisImporter(Importer):
    def parse_receipe(self, html: str) -> Receipe:
        soup = BeautifulSoup(html, features="lxml")

        receipe_container = soup.find("div", attrs={"class": "entry-content"})
        title = soup.title.text.split("•")[0].strip()

        ingredients_container = receipe_container.find(
            "section", attrs={"class": "zutaten"}
        )

        ingredients = []
        servings = None
        for i, (ul, p) in enumerate(
            zip(
                ingredients_container.find_all("ul"),
                ingredients_container.find_all("strong"),
                strict=True,
            )
        ):
            if m := re.match("Zutaten für (.*)", p.text):
                if i == 0 and (m_servings := re.search(r"(\d+)\s+(\w+)", m.group(1))):
                    servings = Servings(
                        amount=int(m_servings.group(1)), unit=m_servings.group(2)
                    )

                ingredients.append(
                    IngredientGroup(
                        name=m.group(1),
                        ingredients=[
                            parse_ingredient(i.text) for i in ul.find_all("li")
                        ],
                    )
                )

        instructions = [
            sibling.text
            for sibling in ingredients_container.next_siblings
            if sibling.name == "p"
        ]

        return Receipe(
            id=-1,
            title=title,
            ingredients=ingredients,
            instructions=instructions,
            servings=servings,
            tags=["importiert", "eat-this.org", "vegan"],
        )
