-- Drop Table Especie
-- Drop Table Muestra

Create Table If Not Exists Especie (
    id Int Not Null Auto_Increment,
    nombre Varchar(50) Not Null,

    Primary Key(id)
);

Create Table If Not Exists Muestra (
    id Int Auto_Increment,
    especie Int Not Null,
    x Float Not Null,
    y Float Not Null,
    dens Int Not Null,
    año Int Not Null,

    Primary Key(id),
    Foreign Key(especie) References Especie(id)
);
