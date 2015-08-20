#!/usr/bin/env python3

import configparser
import PIL.Image


def load_config():
    config = configparser.ConfigParser()
    config.read("config")
    return config["Resizer"]


def main():
    config = load_config()
    img = PIL.Image.open(config["/home/igor/Coding/Library/image.jpg"])
    height = 320
    width = int(img.size[0] * (height / img.size[1]))
    img = img.resize((width, height), PIL.Image.ANTIALIAS)
    img.save(
        config[
            "/home/igor/Coding/Library/static/book/small-covers" + config[
                "name"
            ] + ".jpg"
        ]
    )
    img = PIL.Image.open(config["/home/igor/Coding/Library/image.jpg"])
    height = 480
    width = int(img.size[0] * (height / img.size[1]))
    img = img.resize((width, height), PIL.Image.ANTIALIAS)
    img.save(
        config[
            "/home/igor/Coding/Library/static/book/large-covers" + config[
                "name"
            ] + ".jpg"
            ]
        )


if __name__ == "__main__":
    main()
