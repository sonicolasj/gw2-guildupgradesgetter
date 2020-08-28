module Domain

type Guild = {
    Id: string
    Name: string
    Tag: string
    Level: int
    Aetherium: int
    Favor: int
}

type Item = {
    Id: int
    Name: string
}

type Upgrade = {
    Id: int
    Name: string
    Description: string
    RequiredLevel: int
    Aetherium: int
    Favor: int
    Coins: int
    Prerequisites: int list
    Items: (int * Item) list
}

