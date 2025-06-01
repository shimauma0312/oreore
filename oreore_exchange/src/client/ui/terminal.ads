with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Interfaces; use Interfaces;
with Common.Models; use Common.Models;
with Common.Client; use Common.Client;
with Server.Price_Feed;
with Server.Order_Book;

package Client.Terminal is
   -- ターミナルクライアントタスク
   task type Terminal_Client_Task is new Client_Interface with
      entry Start (Client_Id : in Natural; Name : in String);
   end Terminal_Client_Task;
   
   -- ANSI エスケープシーケンス定義
   ANSI_RESET   : constant String := ASCII.ESC & "[0m";
   ANSI_BOLD    : constant String := ASCII.ESC & "[1m";
   ANSI_RED     : constant String := ASCII.ESC & "[31m";
   ANSI_GREEN   : constant String := ASCII.ESC & "[32m";
   ANSI_YELLOW  : constant String := ASCII.ESC & "[33m";
   ANSI_BLUE    : constant String := ASCII.ESC & "[34m";
   ANSI_MAGENTA : constant String := ASCII.ESC & "[35m";
   ANSI_CYAN    : constant String := ASCII.ESC & "[36m";
   ANSI_WHITE   : constant String := ASCII.ESC & "[37m";
   -- 明るい色バリエーション（宇宙仕様）
   ANSI_BRIGHT_RED     : constant String := ASCII.ESC & "[91m";
   ANSI_BRIGHT_GREEN   : constant String := ASCII.ESC & "[92m";
   ANSI_BRIGHT_YELLOW  : constant String := ASCII.ESC & "[93m";
   ANSI_BRIGHT_BLUE    : constant String := ASCII.ESC & "[94m";
   ANSI_BRIGHT_MAGENTA : constant String := ASCII.ESC & "[95m";
   ANSI_BRIGHT_CYAN    : constant String := ASCII.ESC & "[96m";
   ANSI_BRIGHT_WHITE   : constant String := ASCII.ESC & "[97m";
   ANSI_CLEAR   : constant String := ASCII.ESC & "[2J";
   ANSI_HOME    : constant String := ASCII.ESC & "[H";
   
   -- 画面更新インターバル
   Screen_Update_Interval : constant Duration := 0.2; -- 200ミリ秒
   
private
   -- 履歴ログのサイズ
   History_Size : constant := 10;
   
   -- 取引履歴保持用の配列
   type Trade_Log_Entry is record
      Trade_Info  : Trade;
      Is_Valid    : Boolean := False;
   end record;
   
   type Trade_Log_Array is array (0 .. History_Size - 1) of Trade_Log_Entry;
   
   -- 板表示用の配列
   type Book_Display is record
      Bids      : Server.Order_Book.Natural_Array (0 .. Server.Order_Book.Max_Display_Levels - 1);
      Asks      : Server.Order_Book.Natural_Array (0 .. Server.Order_Book.Max_Display_Levels - 1);
      Bid_Qtys  : Server.Order_Book.Positive_Array (0 .. Server.Order_Book.Max_Display_Levels - 1);
      Ask_Qtys  : Server.Order_Book.Positive_Array (0 .. Server.Order_Book.Max_Display_Levels - 1);
      Levels    : Natural := 0;
   end record;
   
end Client.Terminal;
