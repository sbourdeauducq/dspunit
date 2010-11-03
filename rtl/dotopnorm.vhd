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
use work.dspunit_pac.all;
use work.dspalu_pac.all;
-------------------------------------------------------------------------------
entity dotopnorm is
  port (
    --@inputs
    clk        : in  std_logic;
    op_en      : in  std_logic;
    data_in_m0 : in  std_logic_vector((sig_width - 1) downto 0);
    data_in_m1 : in  std_logic_vector((sig_width - 1) downto 0);
    data_in_m2 : in  std_logic_vector((sig_width - 1) downto 0);
    length_reg : in  std_logic_vector((cmdreg_data_width -1) downto 0);
    offset_params : in  std_logic_vector((cmdreg_data_width -1) downto 0);
    offset_result : in  std_logic_vector((cmdreg_data_width -1) downto 0);
    opflag_select   : in  std_logic_vector((opflag_width - 1) downto 0);
    result1        : in  std_logic_vector((sig_width - 1) downto 0);
    result2        : in  std_logic_vector((sig_width - 1) downto 0);
    cmp_greater    : in  std_logic;
    --@outputs;
    dsp_bus    : out t_dsp_bus
    );
end dotopnorm;
--=----------------------------------------------------------------------------
architecture archi_dotopnorm of dotopnorm is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  constant c_state_pipe_depth : integer := c_dspmem_pipe_depth + 2;
  constant c_dotopnorm_pipe_depth       : integer := c_dspmem_pipe_depth + 6;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_dsp_bus : t_dsp_bus;
  type   t_dotopnorm_state is (st_init, st_load_param1, st_load_param2, st_startpipe, st_compute, st_end);
  signal s_state   : t_dotopnorm_state;
  type t_state_pipe is array(0 to c_state_pipe_depth - 1) of t_dotopnorm_state;
  signal s_state_pipe : t_state_pipe;
  signal s_length  : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_r            : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_w            : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_w_offs       : unsigned((cmdreg_width - 1) downto 0);
  signal s_wr_en             : std_logic;
  signal s_data_a            : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_b            : std_logic_vector((sig_width - 1) downto 0);
  signal s_param1            : std_logic_vector((sig_width - 1) downto 0);
  signal s_param2            : std_logic_vector((sig_width - 1) downto 0);
  signal s_muladd_mode       : std_logic;
  signal s_data_out          : std_logic_vector((cmdreg_width - 1) downto 0);
begin  -- archs_dotopnorm
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_dotopnorm : process (clk)
  begin  -- process p_dotopnorm
    if rising_edge(clk) then            -- rising clock edge
      if op_en = '0' then
        s_state              <= st_init;
        s_dsp_bus.op_done    <= '0';
        s_addr_r  <= (others => '0');
        s_addr_w    <= (others => '0');
        s_wr_en   <= '0';
        -------------------------------------------------------------------------------
        -- operation management
        -------------------------------------------------------------------------------
      else
        case s_state is
          when st_init =>
            s_addr_w   <= (others => '0');
            s_addr_r <= (others => '0');
            s_wr_en  <= '0';
            if s_dsp_bus.op_done = '0' then
              s_state <= st_load_param1;
              s_addr_r <= unsigned(offset_params); -- addr to get first param
            end if;
          when st_load_param1 =>
            s_addr_r <= unsigned(offset_params) + 1; -- addr to get second param
            s_state <= st_load_param2;
          when st_load_param2 =>
            s_addr_r <= (others => '0');    -- init addr counter to start signal reading
            s_state <= st_startpipe;
          when st_startpipe =>
            if s_addr_r = c_dotopnorm_pipe_depth - 1 then
              s_wr_en <= '1';
              s_state            <= st_compute;
            end if;
            s_addr_w <= (others => '0');    --  init addr counter to start signal write
            -- index increment
            s_addr_r <= s_addr_r + 1;
          when st_compute =>
            s_wr_en <= '1';
            if(s_addr_w = s_length) then
              s_wr_en <= '0';
              s_state           <= st_end;
            else
              s_addr_r <= s_addr_r + 1;
              s_addr_w <= s_addr_w + 1; -- and s_length;
            end if;
          when st_end =>
            s_wr_en <= '0';
            s_state <= st_init;
            s_dsp_bus.op_done <= '1';
          when others => null;
        end case;
      end if;
    end if;
  end process p_dotopnorm;

  p_data_select : process (clk)
  begin -- process p_data_select
    if rising_edge(clk) then  -- rising clock edge
      case opflag_select(opflagbit_srcm2 downto opflagbit_srcm0) is
        when "011" =>
          s_data_a <= data_in_m0;
          s_data_b <= data_in_m1;
        when "101" =>
          s_data_a <= data_in_m0;
          s_data_b <= data_in_m2;
        when "110" =>
          s_data_a <= data_in_m1;
          s_data_b <= data_in_m2;
        when others =>
          s_data_a <= data_in_m0;
          s_data_b <= data_in_m1;
      end case;
    end if;
  end process p_data_select;

  p_out_select : process (clk)
  begin -- process p_out_select
    if rising_edge(clk) then  -- rising clock edge
      if op_en = '0' then
        s_dsp_bus.wr_en_m0 <= '0';
        s_dsp_bus.wr_en_m1 <= '0';
        s_dsp_bus.wr_en_m2 <= '0';
      elsif opflag_select(opflagbit_m0) = '1' then
        s_dsp_bus.wr_en_m0 <= s_wr_en;
        s_dsp_bus.wr_en_m1 <= '0';
        s_dsp_bus.wr_en_m2 <= '0';
      elsif opflag_select(opflagbit_m1) = '1' then
        s_dsp_bus.wr_en_m0 <= '0';
        s_dsp_bus.wr_en_m1 <= s_wr_en;
        s_dsp_bus.wr_en_m2 <= '0';
      elsif opflag_select(opflagbit_m2) = '1' then
        s_dsp_bus.wr_en_m0 <= '0';
        s_dsp_bus.wr_en_m1 <= '0';
        s_dsp_bus.wr_en_m2 <= s_wr_en;
      end if;
    end if;
  end process p_out_select;

  p_adr_select : process (clk)
  begin -- process p_adr_select
    if rising_edge(clk) then  -- rising clock edge
      if op_en = '0' then
        s_dsp_bus.addr_r_m0 <= (others => '0');
        s_dsp_bus.addr_w_m0 <= (others => '0');
        s_dsp_bus.addr_m1 <= (others => '0');
        s_dsp_bus.addr_m2 <= (others => '0');
        s_dsp_bus.c_en_m0 <= '0';
        s_dsp_bus.c_en_m1 <= '0';
        s_dsp_bus.c_en_m2 <= '0';
      else
        s_dsp_bus.addr_w_m0 <= s_addr_w_offs;
        s_dsp_bus.addr_r_m0 <= s_addr_r;
        if opflag_select(opflagbit_srcm1) = '1' then
          s_dsp_bus.addr_m1 <= s_addr_r;
        else
          s_dsp_bus.addr_m1 <= s_addr_w_offs;
        end if;
        if opflag_select(opflagbit_srcm2) = '1' then
          s_dsp_bus.addr_m2 <= s_addr_r;
        else
          s_dsp_bus.addr_m2 <= s_addr_w_offs;
        end if;
        s_dsp_bus.c_en_m0 <= opflag_select(opflagbit_srcm0) or opflag_select(opflagbit_m0);
        s_dsp_bus.c_en_m1 <= opflag_select(opflagbit_srcm1) or opflag_select(opflagbit_m1);
        s_dsp_bus.c_en_m2 <= opflag_select(opflagbit_srcm2) or opflag_select(opflagbit_m2);
      end if;
    end if;
  end process p_adr_select;

  p_op_ctrl : process (clk)
  begin -- process p_op_ctrl
    if rising_edge(clk) then  -- rising clock edge
      if s_muladd_mode = '1' then
        -- sum of the two mul outputs
        s_dsp_bus.mul_in_a1 <= s_data_a;
        s_dsp_bus.mul_in_a2 <= s_data_b;
        s_dsp_bus.mul_in_b1 <= s_param1;
        s_dsp_bus.mul_in_b2 <= s_param2;
        s_dsp_bus.acc_mode1 <= acc_sumstore;
      else
        s_dsp_bus.mul_in_a1 <= s_data_a;
        s_dsp_bus.mul_in_b1 <= s_data_b;
        s_dsp_bus.mul_in_a2 <= (others => '0');
        s_dsp_bus.mul_in_b2 <= (others => '0');
        s_dsp_bus.acc_mode1 <= acc_store;
      end if;
    end if;
  end process p_op_ctrl;

  p_data_out : process (clk)
  begin -- process p_data_out
    if rising_edge(clk) then  -- rising clock edge
      s_dsp_bus.data_out_m0 <= s_data_out;
      s_dsp_bus.data_out_m1 <= s_data_out;
      s_dsp_bus.data_out_m2 <= s_data_out;
    end if;
  end process p_data_out;

  p_pipe : process (clk)
  begin -- process p_pipe
    if rising_edge(clk) then  -- rising clock edge
      s_state_pipe(0) <= s_state;
      for i in 0 to c_state_pipe_depth - 2 loop
        s_state_pipe(i + 1) <= s_state_pipe(i);
      end loop;
    end if;
  end process p_pipe;

  p_load_params : process (clk)
  begin -- process p_load_params
    if rising_edge(clk) then  -- rising clock edge
      case s_state_pipe(c_state_pipe_depth - 1) is
        when st_load_param1 => s_param1 <= s_data_b;
        when st_load_param2 => s_param2 <= s_data_b;
        when others => null;
      end case;
    end if;
  end process p_load_params;

  p_data_out_sel : process (s_state,result1)
  begin -- process p_data_out_sel
    case s_state is
      when st_compute => s_data_out <= result1;
      when others => s_data_out <= (others => '0');
    end case;
  end process p_data_out_sel;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  dsp_bus                  <= s_dsp_bus;
  s_dsp_bus.gcounter_reset <= '1';
  s_length                 <= unsigned(length_reg);
  s_dsp_bus.alu_select <= alu_mul;
  s_muladd_mode <= opflag_select(opflagbit_muladd);
  s_addr_w_offs <= s_addr_w + unsigned(offset_result);
end archi_dotopnorm;

