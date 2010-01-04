-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2009, 2010 Dr. Juergen Sauermann
-- 
--  This code is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This code is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this code (see the file named COPYING).
--  If not, see http://www.gnu.org/licenses/.
--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- Module Name:    prog_mem - Behavioral 
-- Create Date:    14:09:04 10/30/2009 
-- Description:    the program memory of a CPU.
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- the content of the program memory.
--
use work.prog_mem_content.all;

entity prog_mem is
    port (  I_CLK       : in  std_logic;

            I_WAIT      : in  std_logic;
            I_PC        : in  std_logic_vector(15 downto 0); -- word address
            I_PM_ADR    : in  std_logic_vector(11 downto 0); -- byte address

            Q_OPC       : out std_logic_vector(31 downto 0);
            Q_PC        : out std_logic_vector(15 downto 0);
            Q_PM_DOUT   : out std_logic_vector( 7 downto 0));
end prog_mem;

architecture Behavioral of prog_mem is

constant zero_256 : bit_vector := X"00000000000000000000000000000000"
                                & X"00000000000000000000000000000000";

component RAMB4_S4_S4
    generic(INIT_00 : bit_vector := zero_256;
            INIT_01 : bit_vector := zero_256;
            INIT_02 : bit_vector := zero_256;
            INIT_03 : bit_vector := zero_256;
            INIT_04 : bit_vector := zero_256;
            INIT_05 : bit_vector := zero_256;
            INIT_06 : bit_vector := zero_256;
            INIT_07 : bit_vector := zero_256;
            INIT_08 : bit_vector := zero_256;
            INIT_09 : bit_vector := zero_256;
            INIT_0A : bit_vector := zero_256;
            INIT_0B : bit_vector := zero_256;
            INIT_0C : bit_vector := zero_256;
            INIT_0D : bit_vector := zero_256;
            INIT_0E : bit_vector := zero_256;
            INIT_0F : bit_vector := zero_256);

    port(   ADDRA   : in  std_logic_vector(9 downto 0);
            ADDRB   : in  std_logic_vector(9 downto 0);
            CLKA    : in  std_ulogic;
            CLKB    : in  std_ulogic;
            DIA     : in  std_logic_vector(3 downto 0);
            DIB     : in  std_logic_vector(3 downto 0);
            ENA     : in  std_ulogic;
            ENB     : in  std_ulogic;
            RSTA    : in  std_ulogic;
            RSTB    : in  std_ulogic;
            WEA     : in  std_ulogic;
            WEB     : in  std_ulogic;

            DOA     : out std_logic_vector(3 downto 0);
            DOB     : out std_logic_vector(3 downto 0));
end component;

signal M_OPC_E      : std_logic_vector(15 downto 0);
signal M_OPC_O      : std_logic_vector(15 downto 0);
signal M_PMD_E      : std_logic_vector(15 downto 0);
signal M_PMD_O      : std_logic_vector(15 downto 0);

signal L_WAIT_N     : std_logic;
signal L_PC_0       : std_logic;
signal L_PC_E       : std_logic_vector(10 downto 1);
signal L_PC_O       : std_logic_vector(10 downto 1);
signal L_PMD        : std_logic_vector(15 downto 0);
signal L_PM_ADR_1_0 : std_logic_vector( 1 downto 0);

begin

    pe_0 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => pe_0_00, INIT_01 => pe_0_01, INIT_02 => pe_0_02, 
                INIT_03 => pe_0_03, INIT_04 => pe_0_04, INIT_05 => pe_0_05,
                INIT_06 => pe_0_06, INIT_07 => pe_0_07, INIT_08 => pe_0_08,
                INIT_09 => pe_0_09, INIT_0A => pe_0_0A, INIT_0B => pe_0_0B, 
                INIT_0C => pe_0_0C, INIT_0D => pe_0_0D, INIT_0E => pe_0_0E,
                INIT_0F => pe_0_0F)
    port map(ADDRA => L_PC_E,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_E(3 downto 0),      DOB   => M_PMD_E(3 downto 0));
 
    pe_1 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => pe_1_00, INIT_01 => pe_1_01, INIT_02 => pe_1_02,
                INIT_03 => pe_1_03, INIT_04 => pe_1_04, INIT_05 => pe_1_05,
                INIT_06 => pe_1_06, INIT_07 => pe_1_07, INIT_08 => pe_1_08,
                INIT_09 => pe_1_09, INIT_0A => pe_1_0A, INIT_0B => pe_1_0B,
                INIT_0C => pe_1_0C, INIT_0D => pe_1_0D, INIT_0E => pe_1_0E,
                INIT_0F => pe_1_0F)
    port map(ADDRA => L_PC_E,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_E(7 downto 4),      DOB   => M_PMD_E(7 downto 4));
 
    pe_2 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => pe_2_00, INIT_01 => pe_2_01, INIT_02 => pe_2_02,
                INIT_03 => pe_2_03, INIT_04 => pe_2_04, INIT_05 => pe_2_05,
                INIT_06 => pe_2_06, INIT_07 => pe_2_07, INIT_08 => pe_2_08,
                INIT_09 => pe_2_09, INIT_0A => pe_2_0A, INIT_0B => pe_2_0B,
                INIT_0C => pe_2_0C, INIT_0D => pe_2_0D, INIT_0E => pe_2_0E,
                INIT_0F => pe_2_0F)
    port map(ADDRA => L_PC_E,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_E(11 downto 8),     DOB   => M_PMD_E(11 downto 8));
 
    pe_3 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => pe_3_00, INIT_01 => pe_3_01, INIT_02 => pe_3_02,
                INIT_03 => pe_3_03, INIT_04 => pe_3_04, INIT_05 => pe_3_05,
                INIT_06 => pe_3_06, INIT_07 => pe_3_07, INIT_08 => pe_3_08,
                INIT_09 => pe_3_09, INIT_0A => pe_3_0A, INIT_0B => pe_3_0B,
                INIT_0C => pe_3_0C, INIT_0D => pe_3_0D, INIT_0E => pe_3_0E,
                INIT_0F => pe_3_0F)
    port map(ADDRA => L_PC_E,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_E(15 downto 12),    DOB   => M_PMD_E(15 downto 12));
 
    po_0 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => po_0_00, INIT_01 => po_0_01, INIT_02 => po_0_02,
                INIT_03 => po_0_03, INIT_04 => po_0_04, INIT_05 => po_0_05,
                INIT_06 => po_0_06, INIT_07 => po_0_07, INIT_08 => po_0_08,
                INIT_09 => po_0_09, INIT_0A => po_0_0A, INIT_0B => po_0_0B, 
                INIT_0C => po_0_0C, INIT_0D => po_0_0D, INIT_0E => po_0_0E,
                INIT_0F => po_0_0F)
    port map(ADDRA => L_PC_O,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_O(3 downto 0),      DOB   => M_PMD_O(3 downto 0));
 
    po_1 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => po_1_00, INIT_01 => po_1_01, INIT_02 => po_1_02,
                INIT_03 => po_1_03, INIT_04 => po_1_04, INIT_05 => po_1_05,
                INIT_06 => po_1_06, INIT_07 => po_1_07, INIT_08 => po_1_08,
                INIT_09 => po_1_09, INIT_0A => po_1_0A, INIT_0B => po_1_0B, 
                INIT_0C => po_1_0C, INIT_0D => po_1_0D, INIT_0E => po_1_0E,
                INIT_0F => po_1_0F)
    port map(ADDRA => L_PC_O,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_O(7 downto 4),      DOB   => M_PMD_O(7 downto 4));
 
    po_2 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => po_2_00, INIT_01 => po_2_01, INIT_02 => po_2_02,
                INIT_03 => po_2_03, INIT_04 => po_2_04, INIT_05 => po_2_05,
                INIT_06 => po_2_06, INIT_07 => po_2_07, INIT_08 => po_2_08,
                INIT_09 => po_2_09, INIT_0A => po_2_0A, INIT_0B => po_2_0B,
                INIT_0C => po_2_0C, INIT_0D => po_2_0D, INIT_0E => po_2_0E,
                INIT_0F => po_2_0F)
    port map(ADDRA => L_PC_O,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_O(11 downto 8),     DOB   => M_PMD_O(11 downto 8));
 
    po_3 : RAMB4_S4_S4 ---------------------------------------------------------
    generic map(INIT_00 => po_3_00, INIT_01 => po_3_01, INIT_02 => po_3_02,
                INIT_03 => po_3_03, INIT_04 => po_3_04, INIT_05 => po_3_05,
                INIT_06 => po_3_06, INIT_07 => po_3_07, INIT_08 => po_3_08,
                INIT_09 => po_3_09, INIT_0A => po_3_0A, INIT_0B => po_3_0B, 
                INIT_0C => po_3_0C, INIT_0D => po_3_0D, INIT_0E => po_3_0E,
                INIT_0F => po_3_0F)
    port map(ADDRA => L_PC_O,                   ADDRB => I_PM_ADR(11 downto 2),
             CLKA  => I_CLK,                    CLKB  => I_CLK,
             DIA   => "0000",                   DIB   => "0000",
             ENA   => L_WAIT_N,                 ENB   => '1',
             RSTA  => '0',                      RSTB  => '0',
             WEA   => '0',                      WEB   => '0',
             DOA   => M_OPC_O(15 downto 12),    DOB   => M_PMD_O(15 downto 12));

    -- remember I_PC0 and I_PM_ADR for the output mux.
    --
    pc0: process(I_CLK)
    begin
        if (rising_edge(I_CLK)) then
            Q_PC <= I_PC;
            L_PM_ADR_1_0 <= I_PM_ADR(1 downto 0);
            if ((I_WAIT = '0')) then
                L_PC_0 <= I_PC(0);
            end if;
        end if;
    end process;

    L_WAIT_N <= not I_WAIT;

    -- we use two memory blocks _E and _O (even and odd).
    -- This gives us a quad-port memory so that we can access
    -- I_PC, I_PC + 1, and PM simultaneously.
    --
    -- I_PC and I_PC + 1 are handled by port A of the memory while PM
    -- is handled by port B.
    --
    -- Q_OPC(15 ... 0) shall contain the word addressed by I_PC, while
    -- Q_OPC(31 ... 16) shall contain the word addressed by I_PC + 1.
    --
    -- There are two cases:
    --
    -- case A: I_PC     is even, thus I_PC + 1 is odd
    -- case B: I_PC + 1 is odd , thus I_PC is even
    --
    L_PC_O <= I_PC(10 downto 1);
    L_PC_E <= I_PC(10 downto 1) + ("000000000" & I_PC(0));
    Q_OPC(15 downto  0) <= M_OPC_E when L_PC_0 = '0' else M_OPC_O;
    Q_OPC(31 downto 16) <= M_OPC_E when L_PC_0 = '1' else M_OPC_O;

    L_PMD <= M_PMD_E               when (L_PM_ADR_1_0(1) = '0') else M_PMD_O;
    Q_PM_DOUT <= L_PMD(7 downto 0) when (L_PM_ADR_1_0(0) = '0')
            else L_PMD(15 downto 8);
    
end Behavioral;

