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
-- Fichier    : rompkg.vhd
-- Module     : PACKAGE rompkg
-- Descript.  :
-- Auteur     : J.Weber
-- Date       : 03/03/07
-- Version    : 1.0
-- Depend.    :
-- Simulation : ModelSim 6.0d
-- Synthèse   :
-- Remarques  :
--
--
-------------------------------------------------------------------------------
-- Date     | Rév | Description
-- 01/08/06 | 1.0 | Première version stable utilisée pour le livre.
-- 03/03/07 |     | Pas de modifications du design.
--          |     | Preparation pour la mise en ligne.
--          |     |
--          |     |
-------------------------------------------------------------------------------


LIBRARY IEEE ;
USE IEEE.STD_LOGIC_1164.ALL ;

PACKAGE rompkg IS
   CONSTANT ad_nb_bits : INTEGER := 3 ; -- e
   CONSTANT size: INTEGER := 2**ad_nb_bits ; -- 8 bytes
   SUBTYPE byte IS STD_LOGIC_VECTOR(7 DOWNTO 0) ;
   TYPE rom_tbl IS ARRAY(NATURAL RANGE <>) OF byte ;
END PACKAGE rompkg ;


