Insert Into Especie (nombre) Values ("Aporrectodea Caliginosa");
Insert Into Especie (nombre) Values ("Aporrectodea Rosea");
Insert Into Especie (nombre) Values ("Aporrectodea Trapezoides");
Insert Into Especie (nombre) Values ("Octolasion Cyaneum");
Insert Into Especie (nombre) Values ("Bimastus Parvus");
Insert Into Especie (nombre) Values ("Amynthas Hawayanus");
Insert Into Especie (nombre) Values ("Eukerria Stagnalis");
Insert Into Especie (nombre) Values ("Microscolex Dubius");
Insert Into Especie (nombre) Values ("Microscolex Phosphoreus");

Select Floor(Rand() * 9 + 1) As especie
From seq_1_to_100;

Select Rand() * (63.39386 - 56.66736) - 63.39386 As x
From seq_1_to_100;

Call generar_data();

Select *
From Muestra
Limit 10;

Select *
From Muestra M
    Join Especie S On M.id = S.especie
