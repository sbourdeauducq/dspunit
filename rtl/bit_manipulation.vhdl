-- bit_manipulation.vhdl - miscellaneous bit manipulation functions
-- Copyright (C) 2001, 2002 Michael Riepe <michael@stud.uni-hannover.de>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-- $Id: bit_manipulation.vhdl,v 1.10 2002/07/05 21:36:57 michael Exp $

-- url : http://f-cpu.seul.org/whygee/f-cpu/f-cpu/vhdl/common/bit_manipulation.vhdl

library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

package Bit_Manipulation is
	-- reverse bits in a vector
	function bit_reverse (A : in signed) return signed;
	function bit_reverse (A : in unsigned) return unsigned;
	function bit_reverse (A : in std_logic_vector) return std_logic_vector;
	-- extract 1 bit of N, starting at offset O
	function bit_extract (A : in std_logic_vector;
						  N : in positive;
						  O : in natural := 0) return std_logic_vector;
	-- duplicate all bits in a vector
	function bit_duplicate (A : in std_logic_vector;
							N : in positive) return std_logic_vector;
	-- duplicate vector
	function vector_duplicate (A : in std_logic_vector;
							   N : in positive) return std_logic_vector;
	-- AND cascade
	function cascade_and (A : in std_logic_vector) return std_logic_vector;
	-- OR cascade
	function cascade_or (A : in std_logic_vector) return std_logic_vector;
	-- n:1 AND
	function reduce_and (A : in std_logic_vector) return std_logic;
	-- n:1 XOR
	function reduce_xor (A : in std_logic_vector) return std_logic;
	-- n:1 OR
	function reduce_or (A : in std_logic_vector) return std_logic;
	-- left shift w/ carry-in
	function lshift (A : in std_logic_vector;
					 N : in natural;
					 C : in std_logic) return std_logic_vector;
	-- left shift w/o carry-in
	function lshift (A : in std_logic_vector;
					 N : in natural) return std_logic_vector;
	-- arithmetic left shift
	function lshifta (A : in std_logic_vector;
					  N : in natural) return std_logic_vector;
	-- right shift w/ carry-in
	function rshift (A : in std_logic_vector;
					 N : in natural;
					 C : in std_logic) return std_logic_vector;
	-- right shift w/o carry-in
	function rshift (A : in std_logic_vector;
					 N : in natural) return std_logic_vector;
	-- arithmetic right shift
	function rshifta (A : in std_logic_vector;
					  N : in natural) return std_logic_vector;
	-- left rotate
	function lrotate (A : in std_logic_vector;
					  N : in natural) return std_logic_vector;
	-- right rotate
	function rrotate (A : in std_logic_vector;
					  N : in natural) return std_logic_vector;
--	function bitbit_and(A : in unsigned; B : in unsigned) return unsigned;
--	function bitbit_and(A : in signed; B : in signed) return signed;
	function bitbit_and(A : in std_logic_vector; B : in std_logic_vector) return std_logic_vector;
end Bit_Manipulation;

package body Bit_Manipulation is
        function bit_reverse (A : in signed) return signed is
	begin
	  return signed(bit_reverse(std_logic_vector(A)));
	end bit_reverse;

        function bit_reverse (A : in unsigned) return unsigned is
	begin
	  return unsigned(bit_reverse(std_logic_vector(A)));
	end bit_reverse;

	function bit_reverse (A : in std_logic_vector) return std_logic_vector is
		constant L : natural := A'length;
		variable aa, yy : std_logic_vector(L-1 downto 0);
	begin
--pragma synthesis_off
		assert L > 0;
--pragma synthesis_on
		aa := A;
		for i in aa'range loop
			yy(i) := aa(L - 1 - i);
		end loop;
		return yy;
	end bit_reverse;

	function bit_extract (A : in std_logic_vector;
						  N : in positive;
						  O : in natural := 0) return std_logic_vector is
		constant L : natural := A'length;
		constant L2 : natural := (L - O + N - 1) / N;
		alias aa : std_logic_vector(L-1 downto 0) is A;
		variable yy : std_logic_vector(L2-1 downto 0);
	begin
--pragma synthesis_off
		assert L > O;
--pragma synthesis_on
		for i in L2-1 downto 0 loop
			yy(i) := aa(N*i+O);
		end loop;
		return yy;
	end bit_extract;

	function bit_duplicate (A : in std_logic_vector;
							N : in positive) return std_logic_vector is
		constant L : natural := A'length;
		alias aa : std_logic_vector(L-1 downto 0) is A;
		variable yy : std_logic_vector(N*L-1 downto 0);
	begin
--pragma synthesis_off
		assert L > 0;
		assert N > 0;
--pragma synthesis_on
		for i in N*L-1 downto 0 loop
			yy(i) := aa(i/N);
		end loop;
		return yy;
	end bit_duplicate;

	function vector_duplicate (A : in std_logic_vector;
							   N : in positive) return std_logic_vector is
		constant L : natural := A'length;
		alias aa : std_logic_vector(L-1 downto 0) is A;
		variable yy : std_logic_vector(N*L-1 downto 0);
	begin
--pragma synthesis_off
		assert L > 0;
		assert N > 0;
--pragma synthesis_on
		for i in N*L-1 downto 0 loop
			yy(i) := aa(i rem L);
		end loop;
		return yy;
	end vector_duplicate;

	function cascade_and (A : in std_logic_vector) return std_logic_vector is
		constant L : natural := A'length;
		variable aa, bb : std_logic_vector(L-1 downto 0);
		variable k1, k2, k3 : integer;
		variable step : natural;
	begin
--pragma synthesis_off
		assert L > 0;
--pragma synthesis_on
		aa := A;
		for i in 0 to 15 loop	-- should be enough
			step := 4 ** i;
			exit when step >= L;
			for j in aa'range loop
				k1 := j - j mod (4 * step) + step - 1;
				k2 := k1 + step;
				k3 := k2 + step;
				case (j / step) mod 4 is
					when 3 =>
						bb(j) := aa(j) and aa(k1) and aa(k2) and aa(k3);
					when 2 =>
						bb(j) := aa(j) and aa(k1) and aa(k2);
					when 1 =>
						bb(j) := aa(j) and aa(k1);
					when others =>
						bb(j) := aa(j);
				end case;
			end loop;
			aa := bb;
		end loop;
		return aa;
	end cascade_and;

	function cascade_or (A : in std_logic_vector) return std_logic_vector is
		constant L : natural := A'length;
		variable aa, bb : std_logic_vector(L-1 downto 0);
		variable k1, k2, k3 : integer;
		variable step : natural;
	begin
--pragma synthesis_off
		assert L > 0;
--pragma synthesis_on
		aa := A;
		for i in 0 to 15 loop	-- should be enough
			step := 4 ** i;
			exit when step >= L;
			for j in aa'range loop
				k1 := j - j mod (4 * step) + step - 1;
				k2 := k1 + step;
				k3 := k2 + step;
				case (j / step) mod 4 is
					when 3 =>
						bb(j) := aa(j) or aa(k1) or aa(k2) or aa(k3);
					when 2 =>
						bb(j) := aa(j) or aa(k1) or aa(k2);
					when 1 =>
						bb(j) := aa(j) or aa(k1);
					when others =>
						bb(j) := aa(j);
				end case;
			end loop;
			aa := bb;
		end loop;
		return aa;
	end cascade_or;

	function reduce_and (A : in std_logic_vector) return std_logic is
		constant L : natural := A'length;
		variable aa : std_logic_vector(L-1 downto 0);
		variable k, len : natural;
	begin
--pragma synthesis_off
		assert L > 0;
--pragma synthesis_on
		aa := A;
		len := L;
		for j in 0 to 15 loop	-- should be enough
			exit when len = 1;
			k := len / 4;
			for i in 0 to k-1 loop
				aa(i) := aa(4*i+0) and aa(4*i+1) and aa(4*i+2) and aa(4*i+3);
			end loop;
			case len mod 4 is
				when 3 => aa(k) := aa(4*k+0) and aa(4*k+1) and aa(4*k+2);
				when 2 => aa(k) := aa(4*k+0) and aa(4*k+1);
				when 1 => aa(k) := aa(4*k+0);
				when others => null;
			end case;
			len := (len + 3) / 4;
		end loop;
		return aa(0);
	end reduce_and;

	function reduce_xor (A : in std_logic_vector) return std_logic is
		constant L : natural := A'length;
		variable aa : std_logic_vector(L-1 downto 0);
		variable k, len : natural;
	begin
--pragma synthesis_off
		assert L > 0;
--pragma synthesis_on
		aa := A;
		len := L;
		for j in 0 to 31 loop	-- should be enough
			exit when len = 1;
			k := len / 2;
			for i in 0 to k-1 loop
				aa(i) := aa(2*i+0) xor aa(2*i+1);
			end loop;
			case len mod 2 is
				when 1 => aa(k) := aa(2*k+0);
				when others => null;
			end case;
			len := (len + 1) / 2;
		end loop;
		return aa(0);
	end reduce_xor;

	function reduce_or (A : in std_logic_vector) return std_logic is
		constant L : natural := A'length;
		variable aa : std_logic_vector(L-1 downto 0);
		variable k, len : natural;
	begin
--pragma synthesis_off
		assert L > 0;
--pragma synthesis_on
		aa := A;
		len := L;
		for j in 0 to 15 loop	-- should be enough
			exit when len = 1;
			k := len / 4;
			for i in 0 to k-1 loop
				aa(i) := aa(4*i+0) or aa(4*i+1) or aa(4*i+2) or aa(4*i+3);
			end loop;
			case len mod 4 is
				when 3 => aa(k) := aa(4*k+0) or aa(4*k+1) or aa(4*k+2);
				when 2 => aa(k) := aa(4*k+0) or aa(4*k+1);
				when 1 => aa(k) := aa(4*k+0);
				when others => null;
			end case;
			len := (len + 3) / 4;
		end loop;
		return aa(0);
	end reduce_or;

	function lshift (A : in std_logic_vector;
					 N : in natural;
					 C : in std_logic) return std_logic_vector is
		constant L : natural := A'length;
		alias aa : std_logic_vector(L-1 downto 0) is A;
		variable yy : std_logic_vector(L-1 downto 0);
	begin
		yy := (others => C);
		if N < L then
			yy(L-1 downto N) := aa(L-N-1 downto 0);
		end if;
		return yy;
	end lshift;

	function lshift (A : in std_logic_vector;
					 N : in natural) return std_logic_vector is
	begin
		return lshift(A, N, '0');
	end lshift;

	function lshifta (A : in std_logic_vector;
					  N : in natural) return std_logic_vector is
	begin
		return lshift(A, N, A(A'right));
	end lshifta;

	function rshift (A : in std_logic_vector;
					 N : in natural;
					 C : in std_logic) return std_logic_vector is
		constant L : natural := A'length;
		alias aa : std_logic_vector(L-1 downto 0) is A;
		variable yy : std_logic_vector(L-1 downto 0);
	begin
		yy := (others => C);
		if N < L then
			yy(L-N-1 downto 0) := aa(L-1 downto N);
		end if;
		return yy;
	end rshift;

	function rshift (A : in std_logic_vector;
					 N : in natural) return std_logic_vector is
	begin
		return rshift(A, N, '0');
	end rshift;

	function rshifta (A : in std_logic_vector;
					  N : in natural) return std_logic_vector is
	begin
		return rshift(A, N, A(A'left));
	end rshifta;

	function lrotate (A : in std_logic_vector;
					  N : in natural) return std_logic_vector is
		constant L : natural := A'length;
		alias aa : std_logic_vector(L-1 downto 0) is A;
		variable yy : std_logic_vector(L-1 downto 0);
	begin
		for i in L-1 downto 0 loop
			yy(i) := aa((i + L - N) rem L);
		end loop;
		return yy;
	end lrotate;

	function rrotate (A : in std_logic_vector;
					  N : in natural) return std_logic_vector is
		constant L : natural := A'length;
		alias aa : std_logic_vector(L-1 downto 0) is A;
		variable yy : std_logic_vector(L-1 downto 0);
	begin
		for i in L-1 downto 0 loop
			yy(i) := aa((i + N) rem L);
		end loop;
		return yy;
	end rrotate;

--	function bitbit_and(A : in signed; B : in signed) return signed is
--	begin
--	  return signed(bitbit_and(std_logic_vector(A), std_logic_vector(B)));
--	end bitbit_and;
--	function bitbit_and(A : in unsigned; B : in unsigned) return unsigned is
--	begin
--	  return unsigned(bitbit_and(std_logic_vector(A), std_logic_vector(B)));
--	end bitbit_and;
	function bitbit_and(A : in std_logic_vector; B : in std_logic_vector) return std_logic_vector is
	  constant L : natural := A'length;
	  alias aa : std_logic_vector((L - 1) downto 0) is A;
	  alias bb : std_logic_vector((L - 1) downto 0) is B;
	  variable yy : std_logic_vector((L - 1) downto 0);
	begin
	  for i in L-1 downto 0 loop
	    yy(i) := aa(i) and bb(i);
	  end loop;
	  return yy;
	end bitbit_and;

end Bit_Manipulation;

-- vi: set ts=4 sw=4 equalprg="fmt -72 -p--": please
