with Ada.Text_IO;
with Ada.Calendar;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

with Server.Tasks.Price_Feed;
with Server.Tasks.Order_Book;
with Common.Models; use Common.Models;
with Common.Client; use Common.Client;

procedure Oreore_Exchange is
   -- ASCII アート バナーの表示 (FR-05 バナー表示)
   procedure Display_Banner is
      use Ada.Text_IO;
   begin
      Put_Line ("┌──────────────────────────────────────────────────────┐");
      Put_Line ("│                                                      │");
      Put_Line ("│      ★★★ 宇宙仕様 オレオレ証券取引所 ★★★        │");
      Put_Line ("│                                                      │");
      Put_Line ("│             COSMIC OREORE EXCHANGE v0.2              │");
      Put_Line ("│                                                      │");
      Put_Line ("│      ロケット: 🚀  月: 🌕  火星: 👽  地球: 💰      │");
      Put_Line ("│                                                      │");
      Put_Line ("└──────────────────────────────────────────────────────┘");
      New_Line;
   end Display_Banner;
   
   -- イキりメーター表示 (FR-06 イキりメーター)
   procedure Display_Excitement_Meter (Trades_Per_Second : Natural) is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      
      Max_Meter_Width : constant := 50;
      Meter_Width : Natural;
      Bar : String (1 .. Max_Meter_Width) := (others => ' ');
   begin
      -- トレード数に基づいてメーター幅を計算 (最大50文字)
      Meter_Width := Natural'Min (Trades_Per_Second * 5, Max_Meter_Width);
      
      -- メーターバーを構築
      for I in 1 .. Meter_Width loop
         Bar (I) := '#';
      end loop;
      
      -- ANSI カラーでメーター表示
      if Trades_Per_Second < 2 then
         -- 青: 取引少なめ
         Put (ASCII.ESC & "[94m[" & Bar & "]" & ASCII.ESC & "[0m");
      elsif Trades_Per_Second < 5 then
         -- 緑: 普通
         Put (ASCII.ESC & "[92m[" & Bar & "]" & ASCII.ESC & "[0m");
      elsif Trades_Per_Second < 10 then
         -- 黄: 活発
         Put (ASCII.ESC & "[93m[" & Bar & "]" & ASCII.ESC & "[0m");
      else
         -- 赤: 超活発
         Put (ASCII.ESC & "[91m[" & Bar & "]" & ASCII.ESC & "[0m");
      end if;
      
      Put_Line (" イキりメーター: " & Trades_Per_Second'Image & " 取引/秒");
   end Display_Excitement_Meter;
   
   -- トレード履歴管理用コンテナ
   package Trade_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Trade);
   
   Recent_Trades : Trade_Vectors.Vector;
   
   -- モックアップクライアント実装
   type Mock_Client is new Client_Interface with record
      Id : Natural;
      Last_Price : Natural := 0;
      Trades : Trade_Vectors.Vector;
   end record;
   
   overriding procedure Price_Update
     (Self : in out Mock_Client; New_Price : Natural);
     
   overriding procedure Trade_Notification
     (Self : in out Mock_Client; Trade_Info : Trade);
   
   procedure Price_Update
     (Self : in out Mock_Client; New_Price : Natural) is
   begin
      Self.Last_Price := New_Price;
   end Price_Update;
   
   procedure Trade_Notification
     (Self : in out Mock_Client; Trade_Info : Trade) is
   begin
      Self.Trades.Append (Trade_Info);
      Recent_Trades.Append (Trade_Info);
   end Trade_Notification;
   
begin
   -- バナー表示 (FR-05)
   Display_Banner;
   Ada.Text_IO.Put_Line ("宇宙仕様取引所サーバーを起動しています...");
   
   -- モックアップクライアント作成と登録
   declare
      Client1 : Client_Access := new Mock_Client'(Id => 1, Last_Price => 0, Trades => Trade_Vectors.Empty_Vector);
      Client2 : Client_Access := new Mock_Client'(Id => 2, Last_Price => 0, Trades => Trade_Vectors.Empty_Vector);
   begin
      -- クライアントを価格フィードとオーダーブックに登録
      Server.Tasks.Price_Feed.Price_Feed_Task.Subscribe (Client1);
      Server.Tasks.Price_Feed.Price_Feed_Task.Subscribe (Client2);
      Server.Tasks.Order_Book.Order_Book_Task.Subscribe (Client1);
      Server.Tasks.Order_Book.Order_Book_Task.Subscribe (Client2);
      
      -- 注文生成用の乱数生成器
      declare
         subtype Order_ID_Range is Unsigned_32 range 1 .. 10_000;
         package Random_ID is new Ada.Numerics.Discrete_Random (Order_ID_Range);
         ID_Gen : Random_ID.Generator;
         
         subtype Qty_Range is Positive range 1 .. 100;
         package Random_Qty is new Ada.Numerics.Discrete_Random (Qty_Range);
         Qty_Gen : Random_Qty.Generator;
         
         -- 約定カウンター
         Trade_Counter : Natural := 0;
         Last_Count_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
         Trades_Per_Second : Natural := 0;
      begin
         -- 乱数シードの初期化
         Random_ID.Reset (ID_Gen);
         Random_Qty.Reset (Qty_Gen);
         
         -- メインループ
         loop
            declare
               -- 1秒ごとの取引数カウントリセット
               Current_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               
               -- 現在価格取得
               Current_Price : Natural;
               
               -- サンプル注文の生成
               New_Order : Order;
               Order_Result : Natural;
               
               -- ランダム注文パラメータ
               Random_Side : Order_Side;
               Random_Type : Order_Type;
               Random_Price : Natural;
            begin
               -- 現在価格の取得
               Server.Tasks.Price_Feed.Price_Feed_Task.Get_Current_Price (Current_Price);
               
               -- ランダム注文の生成
               if Random_ID.Random (ID_Gen) mod 2 = 0 then
                  Random_Side := Buy;
               else
                  Random_Side := Sell;
               end if;
               
               if Random_ID.Random (ID_Gen) mod 3 = 0 then
                  Random_Type := Market;
                  Random_Price := 0;
               else
                  Random_Type := Limit;
                  -- 現在価格の±5%範囲でランダム価格を生成
                  if Random_Side = Buy then
                     -- 買い注文は現在価格より少し低めに
                     Random_Price := Natural (Float (Current_Price) * (0.95 + 0.05 * (Float (Random_ID.Random (ID_Gen)) / Float (Order_ID_Range'Last))));
                  else
                     -- 売り注文は現在価格より少し高めに
                     Random_Price := Natural (Float (Current_Price) * (1.0 + 0.05 * (Float (Random_ID.Random (ID_Gen)) / Float (Order_ID_Range'Last))));
                  end if;
               end if;
               
               New_Order := (
                  Id         => Random_ID.Random (ID_Gen),
                  Side       => Random_Side,
                  Order_Type => Random_Type,
                  Quantity   => Random_Qty.Random (Qty_Gen),
                  Price      => Random_Price,
                  Client_Id  => Natural (Random_ID.Random (ID_Gen) mod 2) + 1
               );
               
               -- 注文の送信
               Server.Tasks.Order_Book.Order_Book_Task.Place_Order (New_Order, Order_Result);
               
               -- 1秒経過したらイキりメーターを更新
               if Ada.Calendar."-" (Current_Time, Last_Count_Time) >= 1.0 then
                  Trades_Per_Second := Natural (Recent_Trades.Length) - Trade_Counter;
                  Trade_Counter := Natural (Recent_Trades.Length);
                  Last_Count_Time := Current_Time;
                  
                  -- イキりメーター表示 (FR-06)
                  Display_Excitement_Meter (Trades_Per_Second);
               end if;
               
               -- 少し待機
               delay 0.1;
            end;
         end loop;
      end;
   end;
   
   Ada.Text_IO.Put_Line ("取引所を終了します");
end Oreore_Exchange;
