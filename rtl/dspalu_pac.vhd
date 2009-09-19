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

  type t_acc_mode is (acc_store, acc_sumstore, acc_diff, acc_add, acc_sub, acc_back_add, acc_minback_add, acc_minback_sub);
  type t_alu_select is (alu_add, alu_muladd, alu_mul, alu_cmul, alu_cmul_conj);
  type t_cmp_mode is (cmp_acc1, cmp_acc2);

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

