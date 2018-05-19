# Mini-Database

#############################
# Implementare baza de date #
#############################

	- O baza de date este o lista, care contine mai multe tabele, fiecare
tabela fiind o lista.
	- O tabela este o lista formata din numele ei si cate o lista pentru
fiecare coloana.
	- O coloana este o lista care contine numele ei pe prima pozitie si in
continuare datele.
	-Exemplu:
(("Category" 
 ("ID" 1 2 3 4 5) 
 ("Category_Name" "Mobiles" "Laptops" "Tablet" "Cameras" "Gaming"))
 ("Product"
 ("ID" 1 1 2 2 3 4 null)
 ("Product_Name" "Nokia" "Samsung" "Hp" "Dell" "Apple" "Nikon" "Playstation")))

##########
# Insert #
##########

	- Am impartit baza de date in doua lista, tabelele pana la cea cautata si
tabelele de la cea cautata pana la sfarsit, iar in final concatenez prima lista
cu tabela modificata si cu tabelele de dupa cea modificata.
	- Am format o noua tabela parcurgand-o pe cea in care vreau sa fac
inserarea. Daca coloana la care ma aflam se regasea printre coloanele din lista
primita ca parametru, adaugam la finalul ei valoarea indicata, altfel adaugam
NULL.

#################
# Simple-Select #
#################

	- Extrag din baza de date tabela de care am nevoie, dupa care o parcurg,
iar cand gasesc coloana pe care o cautam, ii returnez elementele.

##########
# Select #
##########

	- Prima oara parcurg lista de conditii si obtin cate o lista cu liniile 
carerespecta o anumita conditie. Dupa acest pas voi obtine o lista finala prin
intersectia listelor obtinute in prima faza. 
	- Parcurg lista de coloane pe care trebuie sa le afisez. Obtin o lista cu 
elementele de pe pozitiile indicate in lista obtinuta anterior. Daca nu mai am 
de aplicat nicio operatie, o afisez, altfel verific ce fel de operatie este si
o aplic asupra listei de elemente.

##########
# Update #
##########

	- Obtine lista de pozitii care respecta conditiile date.
	- Daca o coloana nu se regasesc in lista values, ramane neschimbata, altfel
parcurg coloana si modific pozitiile indicate in lista de pozitii, pe restul le
las neschimbate.

##########
# Remove #
##########

	- Obtine lista de pozitii care respecta conditiile date.
	- Parcurg toate coloanele si elimin elementele de pe pozitiile indicate in
lista de pozitii.

################
# Natural Join #
################

	- Am cautat coloana comun.
	- Obtin o lista cu elementele comune, dupa care obtin lista cu elementele
dupa care fac afisarea.
	- Aplic conditiile pe aceasta lista.
	- Obtin pozitiile la care se afla elementele.
	- Parcurg lista de coloane pe care trebuie sa le afisez si extrag
elementele indicate de lista de pozitii.
