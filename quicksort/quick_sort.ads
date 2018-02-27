package Quick_Sort is

    subtype Item is Integer;
    subtype Index is Natural;
    type Items is array (Index range <>) of Item;

    procedure Sort (The_Items : in out Items);

end Quick_Sort;  
