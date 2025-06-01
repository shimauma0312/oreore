with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Calendar;
with Interfaces; use Interfaces;
with Common.Models; use Common.Models;
with Common.Client; use Common.Client;

package Server.Tasks.Order_Book is
   -- 注文書タスク定義
   task Order_Book_Task is
      -- 注文受付エントリー
      entry Place_Order (O : in Order; Result : out Natural);
      
      -- クライアント登録エントリー
      entry Subscribe (C : Client_Access);
      
      -- 現在の板情報取得エントリー
      entry Get_Order_Book_Snapshot (
         Top_Bids   : out Natural_Array;
         Top_Asks   : out Natural_Array;
         Bid_Qtys   : out Positive_Array;
         Ask_Qtys   : out Positive_Array;
         Levels     : out Natural);
   end Order_Book_Task;
   
   -- 板表示用の配列型
   type Natural_Array is array (Natural range <>) of Natural;
   type Positive_Array is array (Natural range <>) of Positive;
   
   -- 最大表示レベル数
   Max_Display_Levels : constant := 5;
   
private
   -- 注文コンテナ型
   package Order_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Order);
   
   subtype Order_List is Order_Vectors.Vector;
   
   -- 約定IDカウンター
   Next_Trade_Id : Unsigned_32 := 1;
   
end Server.Tasks.Order_Book;
