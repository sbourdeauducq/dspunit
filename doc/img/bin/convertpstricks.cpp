// convertpstricks.cpp : Defines the entry point for the console application.
//

#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include <fstream.h>
#include <iomanip.h>
#include <string.h>
//#include <direct.h>
#include <sys/types.h>
#include <sys/stat.h>


#define TAILLE_CHAINE 200
#define INSTRU_INSERT "INSERT_FIG"
#define NOM_MODELE "figpstricks.txt"
#define CHEMIN_RESULT "FigResult"


void ImbriqueFich(char *NomModele, char *NomCorps, char *NomSortie);

int main(int argc, char* argv[])
{
	char NomSortie[100] = CHEMIN_RESULT;


	if(argc >= 2)
	{

		//mkdir(NomSortie,777);

		cout << "\nMise en forme de la figure contenue dans" << argv[1] << '\n';
		strcat(NomSortie, "/");
		strcat(NomSortie, argv[1]);
		if(argc == 3)
		{
			ImbriqueFich(argv[2], argv[1], NomSortie);
		} else {
			ImbriqueFich(NOM_MODELE, argv[1], NomSortie);
		}
	} else {
		ImbriqueFich("figpstricks.tex", "fig.tex" , "result.tex");
	}

	return 0;
}


void ImbriqueFich(char *NomModele, char *NomCorps, char *NomSortie)
{
	char Chaine[TAILLE_CHAINE];

    ifstream Corps(NomCorps);
	ifstream Modele(NomModele);
	ofstream Sortie(NomSortie);

	while(!Modele.eof())
	{
		Modele.getline(Chaine,TAILLE_CHAINE);
		cout << Chaine << '\n';
		//if(!strcmp(Chaine, INSTRU_INSERT))
		if(strstr(Chaine, INSTRU_INSERT))
		{
			cout << "--> INSERTION DE LA FIGURE ....\n";
			while(!Corps.eof())
			{
				Corps.getline(Chaine, TAILLE_CHAINE);
				Sortie  << Chaine  << '\n';
			}
		} else {
			Sortie << Chaine << '\n';
		}
	}

	Corps.close();
	Modele.close();
	Sortie.close();

}

