Delimiter //

Create Procedure get_muestras()
Begin
    Select nombre As species, año As year, x, y, dens
    From Especie As S Join Muestra As M
        On S.id = M.especie;
End //

Delimiter ;
