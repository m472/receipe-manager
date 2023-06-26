import pytest

from backend.datamodel import Ingredient
from backend.importer.agnostic import parse_ingredient


@pytest.mark.parametrize(
    ("input_", "result"),
    [
        ("8 große Wirsingblätter", Ingredient(8, "", "große Wirsingblätter", None)),
        ("100 g Glasnudeln", Ingredient(100, "gr", "Glasnudeln", None)),
        ("200 g Tofu", Ingredient(200, "gr", "Tofu", None)),
        ("100 g Wasserkastanien", Ingredient(100, "gr", "Wasserkastanien", None)),
        ("2 Knoblauchzehen", Ingredient(2, "", "Knoblauchzehen", None)),
        ("1 Stück Ingwer (ca. 2 cm)", Ingredient(1, "stk", "Ingwer", "ca. 2 cm")),
        ("4 Frühlingszwiebeln", Ingredient(4, "", "Frühlingszwiebeln", None)),
        ("1/2 Bund Koriander", Ingredient(0.5, "bund", "Koriander", None)),
        ("1 EL Sojasauce", Ingredient(1, "el", "Sojasauce", None)),
        (
            "Saft einer halben Limette",
            Ingredient(None, "", "Saft einer halben Limette", None),
        ),
        ("1 TL Sesamöl", Ingredient(1, "tl", "Sesamöl", None)),
        ("Salz & Pfeffer", Ingredient(None, "", "Salz & Pfeffer", None)),
    ],
)
def test_parse_ingredient(input_: str, result: Ingredient):

    assert parse_ingredient(input_) == result
