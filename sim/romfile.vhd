-------------------------------------------------------------------------------
-- Le langage VHDL : du langage au circuit, du circuit au langage.
-- Copyright (C) Jacques Weber, Sébastien Moutault et Maurice Meaudre, 2006.
--
-- Ce programme est libre, vous pouvez le redistribuer et/ou le modifier selon
-- les termes de la Licence Publique Générale GNU publiée par la Free Software
-- Foundation (version 2 ou bien toute autre version ultérieure choisie par
-- vous).
--
-- Ce programme est distribué car potentiellement utile, mais SANS AUCUNE
-- GARANTIE, ni explicite ni implicite, y compris les garanties de
-- commercialisation ou d'adaptation dans un but spécifique. Reportez-vous à
-- la Licence Publique Générale GNU pour plus de détails.
--
-- Vous devez avoir reçu une copie de la Licence Publique Générale GNU en même
-- temps que ce programme ; si ce n'est pas le cas, écrivez à la Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307,
-- États-Unis.
--
-- jacques.weber@lelangagevhdl.net
-- sebastien.moutautl@lelangagevhdl.net
-------------------------------------------------------------------------------
-- Projet     :
-- Design     :
-- Fichier    : rom_ieee.vhd
-- Module     : ENTITY rom_ieee
-- Descript.  :
-- Auteur     : J.Weber
-- Date       : 03/03/07
-- Version    : 1.0
-- Depend.    : rompkg.vhd
--              rom.txt
-- Simulation : ModelSim 6.0d
-- Synthèse   :
-- Remarques  :
--
--
-------------------------------------------------------------------------------
-- Date     | Rév | Description
-- 01/08/06 | 1.0 | Première version stable utilisée pour le livre.
-- 03/03/07 | 1.1 | Pas de modifications du design.
--          |     | Renomage de l'entité rom en rom_ieee.
--          |     | Preparation pour la mise en ligne.
--          |     |
--          |     |
-------------------------------------------------------------------------------



USE STD.TEXTIO.ALL ;
LIBRARY IEEE ;
USE IEEE.STD_LOGIC_1164.ALL, IEEE.NUMERIC_STD.ALL ;
USE IEEE.STD_LOGIC_TEXTIO.ALL ;
USE WORK.rompkg.all ; --le même que précédemment

ENTITY rom_ieee IS
   PORT(adresse : IN STD_LOGIC_VECTOR(ad_nb_bits - 1 DOWNTO 0) ;
        donnee : OUT STD_LOGIC_VECTOR(7 DOWNTO 0) ) ;
END rom_ieee ;

ARCHITECTURE fichier_ieee OF rom_ieee IS
   SIGNAL rom_val : rom_tbl(0 TO taille - 1) ;
BEGIN
lecture : PROCESS
   FILE donnees : TEXT IS "rom.txt" ;
   VARIABLE ligne,message : LINE ;
   VARIABLE i : NATURAL := 0 ;
   VARIABLE rom_buf : octet ; -- type IEEE
   VARIABLE ok : BOOLEAN ;
BEGIN
   WHILE NOT ENDFILE(donnees) LOOP
      READLINE(donnees,ligne);
      HREAD(ligne,rom_buf,ok) ; -- lecture hexadécimal
      IF ok THEN
         rom_val(i) <= rom_buf ; -- pas de conversion
      ELSE
         WRITE(message,STRING'(" !!! Format !!! : ")) ;
         WRITE(message, ligne.ALL) ;
         WRITE(message,STRING'(" à la ligne numéro ")) ;
         WRITE(message,i) ; -- numéro de la ligne fausse
         REPORT message.ALL ; -- affichage console
      END IF ;
         IF i < taille - 1 THEN
            i := i + 1 ;
         ELSE
            REPORT "rom pleine" ;
            EXIT ;
         END IF ;
      END LOOP ;
      WAIT ;
END PROCESS lecture ;
   donnee <= rom_val(TO_INTEGER(UNSIGNED(adresse))) ;
END fichier_ieee ;


