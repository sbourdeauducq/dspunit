--   ----------------------------------------------------------------------
--   DspUnit : Advanced So(P)C Sequential Signal Processor
--   Copyright (C) 2007-2009 by Adrien LELONG (www.lelongdunet.com)
--
--   This program is free software; you can redistribute it and/or modify
--   it under the terms of the GNU General Public License as published by
--   the Free Software Foundation; either version 2 of the License, or
--   (at your option) any later version.
--
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU General Public License for more details.
--
--   You should have received a copy of the GNU General Public License
--   along with this program; if not, write to the
--   Free Software Foundation, Inc.,
--   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
--   ----------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-------------------------------------------------------------------------------

package dspalu_pac is

  function zeros(size   : natural) return unsigned;
  function zeros(size   : natural) return std_logic_vector;
  function sig_one(size : natural) return signed;
  function sig_one(size : natural) return std_logic_vector;

  -- type t_acc_mode is (acc_store, acc_sumstore, acc_diff, acc_add, acc_sub, acc_back_add, acc_minback_sub);
  constant acc_mode_width  : natural := 4;
  -- alias t_acc_mode is std_logic_vector(3 downto 0);
  constant acc_none        : std_logic_vector((acc_mode_width - 1) downto 0) := x"0";
  constant acc_store       : std_logic_vector((acc_mode_width - 1) downto 0) := x"1";
  constant acc_sumstore    : std_logic_vector((acc_mode_width - 1) downto 0) := x"2";
  constant acc_diff        : std_logic_vector((acc_mode_width - 1) downto 0) := x"3";
  constant acc_add         : std_logic_vector((acc_mode_width - 1) downto 0) := x"4";
  constant acc_sub         : std_logic_vector((acc_mode_width - 1) downto 0) := x"5";
  constant acc_back_add    : std_logic_vector((acc_mode_width - 1) downto 0) := x"6";
  constant acc_minback_sub : std_logic_vector((acc_mode_width - 1) downto 0) := x"8";

  -- type t_alu_select is (alu_add, alu_muladd, alu_mul, alu_cmul, alu_cmul_conj);
  -- alias t_alu_select is std_logic_vector(3 downto 0);
  constant alu_select_width : natural := 4;
  -- constant alu_add       : std_logic_vector(3 downto 0) := "0000";
  constant alu_none      : std_logic_vector((alu_select_width - 1) downto 0) := "0000";
  constant alu_muladd    : std_logic_vector((alu_select_width - 1) downto 0) := "0001";
  constant alu_mul       : std_logic_vector((alu_select_width - 1) downto 0) := "0010";
  constant alu_cmul      : std_logic_vector((alu_select_width - 1) downto 0) := "0100";
  constant alu_cmul_conj : std_logic_vector((alu_select_width - 1) downto 0) := "1000";

  -- type t_cmp_mode is (cmp_acc1, cmp_acc2);
  constant cmp_mode_width : natural := 2;
  -- alias t_cmp_mode is std_logic_vector(1 downto 0);
  constant cmp_none : std_logic_vector((cmp_mode_width - 1) downto 0) := "00";
  constant cmp_acc1 : std_logic_vector((cmp_mode_width - 1) downto 0) := "01";
  constant cmp_acc2 : std_logic_vector((cmp_mode_width - 1) downto 0) := "10";

end dspalu_pac;

package body dspalu_pac is

  function zeros(size : natural) return unsigned
  is
    variable vect_zeros : unsigned((size - 1) downto 0);
  begin
    vect_zeros := (others => '0');
    return vect_zeros;
  end zeros;

  function zeros(size : natural) return std_logic_vector
  is
    variable vect_zeros : std_logic_vector((size - 1) downto 0);
  begin
    vect_zeros := (others => '0');
    return vect_zeros;
  end zeros;

  function sig_one(size : natural) return signed
  is
    variable vect_one : signed((size - 1) downto 0);
  begin
    vect_one(size - 1)            := '0';
    vect_one((size - 2) downto 0) := (others => '1');

    return vect_one;
  end sig_one;

  function sig_one(size : natural) return std_logic_vector
  is
    variable vect_one : std_logic_vector((size - 1) downto 0);
  begin
    vect_one(size - 1)            := '0';
    vect_one((size - 2) downto 0) := (others => '1');

    return vect_one;
  end sig_one;


end dspalu_pac;

