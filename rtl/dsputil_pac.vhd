--   ----------------------------------------------------------------------
--   DspUnit : Advanced So(P)C Sequential Signal Processor
--   Copyright (C) 2007-2010 by Adrien LELONG (www.lelongdunet.com)
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

package dsputil_pac is

  function bit_extent(val : std_logic; size :natural) return std_logic_vector;
  function dsp_abs(val : signed) return signed;
  function dsp_abs(val : std_logic_vector) return std_logic_vector;
  function ones(size   : natural) return unsigned;
  function ones(size   : natural) return std_logic_vector;
  function zeros(size   : natural) return unsigned;
  function zeros(size   : natural) return std_logic_vector;
  function sig_one(size : natural) return unsigned;
  function sig_one(size : natural) return signed;
  function sig_one(size : natural) return std_logic_vector;


end dsputil_pac;

package body dsputil_pac is

  function bit_extent(val : std_logic; size :natural) return std_logic_vector
  is
    variable vect_out : std_logic_vector((size - 1) downto 0);
  begin
    vect_out := (others => val);
    return vect_out;
  end bit_extent;

  function dsp_abs(val : signed) return signed
  is
    constant numlength : natural := val'length;
    alias val_in : signed((numlength - 1) downto 0) is val;
    variable val_out : signed((numlength - 1) downto 0);
  begin
    if val_in(numlength - 1) = '0' then
      val_out := val_in;
    else
      val_out := -val_in;
    end if;
    return val_out;
  end dsp_abs;

  function dsp_abs(val : std_logic_vector) return std_logic_vector
  is
    constant numlength : natural := val'length;
    variable val_out : std_logic_vector((numlength - 1) downto 0);
  begin
    val_out := std_logic_vector(dsp_abs(signed(val)));
    return val_out;
  end dsp_abs;

  function ones(size : natural) return unsigned
  is
    variable vect_ones : unsigned((size - 1) downto 0);
  begin
    vect_ones := (others => '1');
    return vect_ones;
  end ones;

  function ones(size : natural) return std_logic_vector
  is
    variable vect_ones : std_logic_vector((size - 1) downto 0);
  begin
    vect_ones := (others => '1');
    return vect_ones;
  end ones;

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

  function sig_one(size : natural) return unsigned
  is
    variable vect_one : unsigned((size - 1) downto 0);
  begin
    vect_one((size - 1) downto 0) := (others => '1');

    return vect_one;
  end sig_one;

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


end dsputil_pac;

