from __future__ import annotations

import re
from dataclasses import asdict
from typing import TYPE_CHECKING

import requests
from flask import (
    Flask,
    jsonify,
    redirect,
    render_template,
    request,
    send_from_directory,
    url_for,
)
from serde import SerdeError
from serde.json import from_json

from backend.datamodel import COOKBOOK, RECEIPE_DIR, RECEIPE_IMG_DIR, Receipe, Servings
from backend.importer.bianca_zapatka import BiancaZapatkaImporter

if TYPE_CHECKING:
    from werkzeug.wrappers.response import Response


app = Flask(__name__)


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
                "tags": receipe.tags,
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
        _id = int(request.args["id"])
        receipe = from_json(Receipe, request.data.decode("utf-8"))
        assert receipe.id == _id

        # delete unreferenced images
        for img_path in RECEIPE_IMG_DIR.iterdir():
            m = re.match(f"{receipe.id}_(\\d+)", img_path.stem)
            if m and int(m.group(1)) not in receipe.image_ids:
                img_path.unlink()

        receipe.save()

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


@app.route("/receipe/import", methods=["POST"])
def import_receipe() -> tuple[Response, int] | Response:

    url = request.args["url"]
    _id = max(COOKBOOK.receipes) + 1

    m = re.match(r"(https?://)?(www.)?([a-z0-9\.\-]+)(/.*)?", url)
    if m:
        print(m.group(3))

    match m.group(3) if m else None:

        case "biancazapatka.com":
            importer = BiancaZapatkaImporter()

        case None:
            msg = f"could not parse url '{url}'"
            print(msg)
            return (jsonify({"status", msg}), 400)

        case domain:
            msg = f"no import handler for domain '{domain}'"
            print(msg)
            return (
                jsonify({"status", msg}),
                400,
            )

    response = requests.get(url)
    assert response.ok

    receipe = importer.parse_receipe(response.text)

    # save imported receipe
    receipe.id = _id
    receipe.save()

    return redirect(url_for(json_receipe.__name__, id=receipe.id))


@app.route("/receipe/create", methods=["POST"])
def json_receipe_create() -> tuple[Response, int]:
    _id = max(COOKBOOK.receipes) + 1
    new_receipe = Receipe(
        id=_id,
        title="",
        ingredients=[],
        servings=Servings(4, "Portionen"),
        instructions=[],
    )

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


if __name__ == "__main__":
    app.run()
