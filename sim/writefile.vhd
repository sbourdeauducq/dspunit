
-------------------------------------------------------------------------------
-- Projet     :  http://www.lelangagevhdl.net/rom_rom_ieee.vhd.html
-- Design     :
-- Fichier    : rom_ieee.vhd
-- Module     : ENTITY rom_ieee_uTest
-- Descript.  : Test unitaire pour l'entité rom_ieee.
-- Auteur     : J.Weber
-- Date       : 03/03/07
-- Version    : 1.0
-- Depend.    : ../do/rom_ieee.do
--              rompkg.vhd
-- Simulation : ModelSim 6.0d
-- Synthèse   :
-- Remarques  : Test non exhaustif par visualisation (pas d'assertion).
--              Simulation fonctionnelle uniquement.
--
-------------------------------------------------------------------------------
-- Date     | Rév | Description
-- 01/08/06 | 1.0 | Première version stable utilisée pour le livre.
-- 03/03/07 | 1.1 | Pas de modifications du design.
--          |     | Entité de test renomé rom_ieee_uTest pour conformité avec
--          |     | la convention de nomage de VhdlDoc.
--          |     | Preparation pour la mise en ligne.
--          |     |
--          |     |
-------------------------------------------------------------------------------
USE STD.TEXTIO.ALL ;
LIBRARY IEEE ;
USE IEEE.STD_LOGIC_1164.ALL, IEEE.NUMERIC_STD.ALL ;
USE IEEE.STD_LOGIC_TEXTIO.ALL ;
USE WORK.rompkg.ALL ; --le même que précédemment

ENTITY rom_ieee_uTest IS
END rom_ieee_uTest ;

ARCHITECTURE fichier_ieee OF rom_ieee_uTest IS
   COMPONENT rom_ieee IS
      PORT(adresse :
                IN STD_LOGIC_VECTOR(ad_nb_bits - 1 DOWNTO 0) ;
           donnee : OUT STD_LOGIC_VECTOR(7 DOWNTO 0) ) ;
   END COMPONENT ;
   SIGNAL rom_val : STD_LOGIC_VECTOR(0 TO taille - 1) ;
   SIGNAL rom_ad : STD_LOGIC_VECTOR(ad_nb_bits - 1 DOWNTO 0)
                 := (OTHERS => '0') ;
BEGIN
DUT : rom_ieee
      PORT MAP(adresse => rom_ad ,
              donnee => rom_val ) ;

ecriture : PROCESS
   FILE donnees : TEXT OPEN WRITE_MODE IS "rom_contenu.txt" ;
   VARIABLE ligne,message : LINE ;
BEGIN
   WAIT FOR 2 ns ; -- attend l’initialisation de la rom
   WRITE(message,STRING'("adresse : donnee ")) ;
   WRITELINE(donnees,message) ;
   WRITE(message,STRING'("octal     binaire  ; hex ")) ;
   WRITELINE(donnees,message) ;
   FOR i IN 0 TO 2**ad_nb_bits - 1 LOOP
      rom_ad <= STD_LOGIC_VECTOR(TO_UNSIGNED(i,3)) ;
      WAIT FOR 2 ns ; -- pour l’actualisation des signaux
      OWRITE(ligne, rom_ad) ; -- format octal
      WRITE(ligne, STRING'("       : ")) ;
      WRITE(ligne, rom_val) ; -- format binaire
      WRITE(ligne, STRING'(" ; ")) ;
      HWRITE(ligne, rom_val) ; -- format hexadécimal
      WRITELINE(donnees,ligne);
   END LOOP ;
   FILE_CLOSE(donnees) ;
   WAIT ;
END PROCESS ecriture ;
END fichier_ieee ;

