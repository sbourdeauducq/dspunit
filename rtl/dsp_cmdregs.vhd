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
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use work.dspalu_pac.all;
use work.dspunit_pac.all;
-------------------------------------------------------------------------------

entity dsp_cmdregs is
  port (
    clk             : in  std_logic;
    clk_cpu         : in  std_logic;
    reset           : in  std_logic;
    -- memory 0
    op_done         : in  std_logic;
    addr_cmdreg     : in  std_logic_vector((cmdreg_addr_width - 1) downto 0);
    data_in_cmdreg  : in  std_logic_vector((cmdreg_data_width - 1) downto 0);
    wr_en_cmdreg    : in  std_logic;
    data_out_cmdreg : out std_logic_vector((cmdreg_data_width - 1) downto 0);
    offset_0        : out unsigned((cmdreg_width - 1) downto 0);
    offset_1        : out unsigned((cmdreg_width - 1) downto 0);
    offset_2        : out unsigned((cmdreg_width - 1) downto 0);
    length0         : out std_logic_vector((cmdreg_data_width - 1) downto 0);
    length1         : out std_logic_vector((cmdreg_data_width - 1) downto 0);
    length2         : out std_logic_vector((cmdreg_data_width - 1) downto 0);
    opflag_select   : out std_logic_vector((opflag_width - 1) downto 0);
    opcode_select   : out std_logic_vector((opcode_width - 1) downto 0);
    debug           : out std_logic_vector(15 downto 0)
    );
end dsp_cmdregs;
--=----------------------------------------------------------------------------
architecture archi_dsp_cmdregs of dsp_cmdregs is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  constant c_refresh_cmdreg_length : integer := 10;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  component dsp_cmdpipe
    port (
      reset   : in  std_logic;
      clk     : in  std_logic;
      cmd_out : out t_dsp_cmdregs;
      read    : in  std_logic;
      empty   : out std_logic;
      cmd_in  : in  t_dsp_cmdregs;
      write   : in  std_logic;
      full    : out std_logic
      );
  end component;
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_dsp_cmdregs         : t_dsp_cmdregs;
  signal s_dsp_cmdregs_buf     : t_dsp_cmdregs;
  signal s_dsp_cmdpipe_out     : t_dsp_cmdregs;
  signal s_dsp_bus             : t_dsp_bus;
  signal s_dsp_bus_conv_circ   : t_dsp_bus;
  signal s_op_conv_circ_en     : std_logic;
  signal s_opflag_select_inreg : std_logic_vector((opflag_width - 1) downto 0);
  signal s_opcode_select_inreg : std_logic_vector((opcode_width - 1) downto 0);
  signal s_op_run_resync       : std_logic;
  signal s_op_run_sync         : std_logic;
  signal s_op_done_sync        : std_logic;
  signal s_op_done_resync      : std_logic;
  signal s_lut_out             : std_logic_vector((lut_out_width - 1) downto 0);
  signal s_load_pipe           : std_logic;
  signal s_status_reg          : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_read                : std_logic;
  signal s_empty               : std_logic;
  signal s_write               : std_logic;
  signal s_full                : std_logic;
  signal s_run_flag            : std_logic;
  signal s_pipe_loaded         : std_logic;
  signal s_op_run              : std_logic;
begin  -- archs_dsp_cmdregs
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  dsp_cmdpipe_1 : dsp_cmdpipe
    port map (
      reset   => reset,
      clk     => clk_cpu,
      cmd_out => s_dsp_cmdpipe_out,
      read    => s_read,
      empty   => s_empty,
      cmd_in  => s_dsp_cmdregs_buf,
      write   => s_write,
      full    => s_full);

  --=---------------------------------------------------------------------------
  -------------------------------------------------------------------------------
  -- Register bank accessibel from controler
  -------------------------------------------------------------------------------
  p_cmdreg_buf : process (clk_cpu, reset)
  begin  -- process p_cmdreg_buf
    if reset = '0' then
      s_dsp_cmdregs_buf <= dsp_cmdregs_init;
    elsif rising_edge(clk_cpu) then     -- rising clock edge
      if(wr_en_cmdreg = '1') then
        s_dsp_cmdregs_buf(conv_integer(addr_cmdreg)) <= data_in_cmdreg;
      else
        s_dsp_cmdregs_buf(DSPADDR_SR) <= s_status_reg;
      end if;
      data_out_cmdreg <= s_dsp_cmdregs_buf(conv_integer(addr_cmdreg));
    end if;
  end process p_cmdreg_buf;
  -------------------------------------------------------------------------------
  -- Bits of status register (readable from cpu)
  -------------------------------------------------------------------------------
  s_status_reg(DSP_SRBIT_LOADED)                         <= not s_full;
  s_status_reg(DSP_SRBIT_OPDONE)                         <= s_op_done_resync;
  s_status_reg(DSP_SRBIT_RUN)                            <= s_run_flag and (not s_load_pipe);
  s_status_reg(cmdreg_width - 1 downto DSP_SRBIT_UNUSED) <= (others => '0');
  s_run_flag                                             <= s_dsp_cmdregs_buf(DSPADDR_SR)(DSP_SRBIT_RUN);
  -------------------------------------------------------------------------------
  -- Control injection of datas in pipe
  -------------------------------------------------------------------------------
  p_ctrl_pipe : process (clk_cpu)
  begin  -- process p_cmdreg_buf
    if rising_edge(clk_cpu) then        -- rising clock edge
      if s_load_pipe = '1' then
        s_write       <= '1';
        s_pipe_loaded <= '1';
      elsif s_run_flag = '0' then
        s_pipe_loaded <= '0';
        s_write       <= '0';
      else
        s_write <= '0';
      end if;
    end if;
  end process p_ctrl_pipe;
  s_load_pipe <= s_run_flag and (not s_pipe_loaded) and (not s_full);
  -------------------------------------------------------------------------------
  -- Control the pipe output
  -------------------------------------------------------------------------------
  p_pipe_out : process (clk_cpu, reset)
  begin  -- process p_pipe_out
    if reset = '0' then
      s_op_run <= '0';
    elsif rising_edge(clk) then         -- rising clock edge
      if s_op_done_resync = '1' then
        s_op_run <= '0';
        s_read   <= '0';
      elsif s_op_run = '0' and s_empty = '0' then
        s_read        <= '1';
        s_op_run      <= '1';
        s_dsp_cmdregs <= s_dsp_cmdpipe_out;
      else
        s_read <= '0';
      end if;
      s_op_done_sync   <= op_done;
      s_op_done_resync <= s_op_done_sync;
    end if;
  end process p_pipe_out;
  -------------------------------------------------------------------------------
  -- Synchronization of command signals to the dspunit clock
  -------------------------------------------------------------------------------
  p_synccmd : process (clk)
  begin  -- process p_synccmd
    if rising_edge(clk) then            -- rising clock edge
      s_op_run_sync   <= s_op_run;
      s_op_run_resync <= s_op_run_sync;
      -- cmdregs can be considered as stable when s_op_run_resync='1'
      if s_op_run_resync = '1' then
        s_opcode_select_inreg <= s_dsp_cmdregs(DSPADDR_OPCODE)((opcode_width - 1) downto 0);
        s_opflag_select_inreg <= s_dsp_cmdregs(DSPADDR_OPCODE)((opflag_width + opcode_width - 1) downto (opcode_width));
      else
        s_opcode_select_inreg <= (others => '0');
        s_opflag_select_inreg <= (others => '0');
      end if;

      opcode_select <= s_opcode_select_inreg;
      opflag_select <= s_opflag_select_inreg;
      offset_0      <= unsigned(s_dsp_cmdregs(DSPADDR_STARTADDR0));
      offset_1      <= unsigned(s_dsp_cmdregs(DSPADDR_STARTADDR1));
      offset_2      <= unsigned(s_dsp_cmdregs(DSPADDR_STARTADDR2));
      length0       <= s_dsp_cmdregs(DSPADDR_LENGTH0);
      length1       <= s_dsp_cmdregs(DSPADDR_LENGTH1);
      length2       <= s_dsp_cmdregs(DSPADDR_LENGTH2);
    end if;
  end process p_synccmd;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  debug <= s_dsp_cmdregs(DSPADDR_SR);
end archi_dsp_cmdregs;
-------------------------------------------------------------------------------
