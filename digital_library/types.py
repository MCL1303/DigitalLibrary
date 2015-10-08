from enum import Enum


class Action(Enum):
    # pylint: disable=no-init
    Take = 1
    Return = 2


class ClientType(Enum):
    # pylint: disable=no-init
    User = 1
    Terminal = 2


class AccessLevel(Enum):
    # pylint: disable=no-init
    Student = 1
    Librarian = 2
