Delimiter //

Create Procedure generar_data()
Begin
    -- Inicializar tabla de muestras:
    Delete From Muestra;
    Alter Table Muestra Auto_Increment 1;

    Insert Into Muestra(especie, x, y, dens, año)
    Select
        Floor(Rand() * 9 + 1),
        Rand() * (63.39386 - 56.66736) - 63.39386,
        Rand() * (41.03542 - 33.26014) - 41.03542,
        Floor(Rand() * 200 + 1),
        Floor(Rand() * 26 + 1988)
    From seq_1_to_100;

End //

Delimiter ;
