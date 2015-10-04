import PIL.Image


def resize(path, folder, name, ext):
    img = PIL.Image.open(path)
    width = int(img.size[0] * (480 / img.size[1]))
    img = img.resize((width, 480), PIL.Image.ANTIALIAS)
    img.save(
        "static/images/{folder}/large-covers/{name}.{ext}"
        .format(folder=folder, name=name, ext=ext)
    )
    width = int(img.size[0] * (320 / img.size[1]))
    img = img.resize((width, 320), PIL.Image.ANTIALIAS)
    img.save(
        "static/images/{folder}/small-covers/{name}.{ext}"
        .format(folder=folder, name=name, ext=ext)
    )
