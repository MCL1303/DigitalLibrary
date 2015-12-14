import random


def main():
    cityes = []
    for _ in range(10):
        cityes += [[random() % 10, random() % 10]]


if __name__ == '__main__':
    main()
