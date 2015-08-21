import configparser
import PIL.Image


class resizer():
    def __init__(self, name, exe):
        img = PIL.Image.open("/home/igor/Coding/Library/{name}.{exe}".format(name=name, exe=exe))
        width = int(img.size[0] * (480 / img.size[1]))
        img = img.resize((width, 480), PIL.Image.ANTIALIAS)
        img.save(
            "/home/igor/Coding/Library/static/book/large-covers/{name}.{exe}"
            .format(name=name, exe=exe)
        )
        width = int(img.size[0] * (320 / img.size[1]))
        img = img.resize((width, 320), PIL.Image.ANTIALIAS)
        img.save(
            "/home/igor/Coding/Library/static/book/small-covers/{name}.{exe}"
            .format(name=name, exe=exe)
        )
