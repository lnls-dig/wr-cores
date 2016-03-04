library ieee;
use ieee.std_logic_1164.all;

entity escape_detector is
  generic(
    g_data_width  : integer;
    g_escape_code : std_logic_vector
    );
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic;


    d_i               : in  std_logic_vector(g_data_width-1 downto 0);
    d_detect_enable_i : in  std_logic;
    d_valid_i         : in  std_logic;
    d_req_o           : out std_logic;

    d_o        : out std_logic_vector(g_data_width-1 downto 0);
    d_escape_o : out std_logic;
    d_valid_o  : out std_logic;
    d_req_i    : in  std_logic
    );

end escape_detector;

architecture behavioral of escape_detector is

  type t_state is (IDLE, CHECK_ESCAPE);

  signal state          : t_state;
  signal is_escape_code : std_logic;
  
begin  -- behavioral

  d_req_o <= d_req_i;

  is_escape_code <= '1' when (d_detect_enable_i = '1' and state = IDLE and d_valid_i = '1' and d_i = g_escape_code) else '0';

  d_o <= g_escape_code when (state = CHECK_ESCAPE and d_i = x"0000") else d_i;

  d_valid_o  <= d_valid_i and not is_escape_code;
  d_escape_o <= '1' when (state = CHECK_ESCAPE and d_i /= x"0000") else '0';

  p_fsm : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' or d_detect_enable_i = '0' then
        state <= IDLE;
      else
        case state is
          when IDLE =>
            if(d_i = g_escape_code and d_valid_i = '1') then
              state <= CHECK_ESCAPE;

            end if;

          when CHECK_ESCAPE =>
            if(d_valid_i = '1') then
              state <= IDLE;
            end if;
        end case;
      end if;
    end if;
  end process;

end behavioral;

