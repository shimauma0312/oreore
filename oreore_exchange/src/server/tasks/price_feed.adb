with Ada.Numerics.Discrete_Random;
with Ada.Calendar;
with Ada.Text_IO;
with Common.Client; use Common.Client;

package body Server.Tasks.Price_Feed is

   task body Price_Feed_Task is
      -- 価格生成用の乱数生成器
      subtype Price_Range is Natural range 100 .. 999;
      package Random_Price is new Ada.Numerics.Discrete_Random (Price_Range);
      Gen : Random_Price.Generator;
      
      -- 現在の価格と関連変数
      Current_Price : Natural := 500; -- 初期価格
      Clients : Client_List;
      Next_Update : Ada.Calendar.Time;
      Update_Interval : constant Duration := 1.0; -- 1秒ごとに更新
   begin
      -- 乱数シードの初期化
      Random_Price.Reset (Gen);
      
      Next_Update := Ada.Calendar.Clock + Update_Interval;
      
      loop
         select
            -- クライアント登録
            accept Subscribe (C : Client_Access) do
               Clients.Append (C);
               -- 新規クライアントに最新価格を通知
               C.Price_Update (Current_Price);
            end Subscribe;
         or
            -- 現在の価格取得
            accept Get_Current_Price (Price : out Natural) do
               Price := Current_Price;
            end Get_Current_Price;
         or
            delay until Next_Update;
            
            
            -- 新価格の生成（FR-01: 乱数価格を1秒間隔で生成）
            declare
               -- 前回価格から±5%の範囲でランダムに変化
               Min_Price : constant Natural := Natural'Max (100, Natural (Float (Current_Price) * 0.95));
               Max_Price : constant Natural := Natural'Min (999, Natural (Float (Current_Price) * 1.05));
               Price_Range : Integer := Max_Price - Min_Price;
               Price_Delta : Integer;
            begin
               -- 乱数での価格変動を計算
               Price_Delta := Random_Price.Random (Gen) mod 11 - 5;  -- -5から+5の範囲
               
               -- 価格を更新（範囲内に収める）
               Current_Price := Natural'Max (100, Natural'Min (999, 
                              Current_Price + Price_Delta * (Price_Range / 50)));
               
               -- クライアントへ通知
               for I in 0 .. Natural (Clients.Length) - 1 loop
                  if Clients (I) /= null then
                     Clients (I).Price_Update (Current_Price);
                  end if;
               end loop;
               
               -- 宇宙仕様のログ表示
               Ada.Text_IO.Put (ASCII.ESC & "[96m" & "💫 Price Feed: " & ASCII.ESC & "[0m");
               if Price_Delta > 0 then
                  Ada.Text_IO.Put_Line (ASCII.ESC & "[92m" & "↑ " & Current_Price'Image & " 🚀" & ASCII.ESC & "[0m");
               elsif Price_Delta < 0 then
                  Ada.Text_IO.Put_Line (ASCII.ESC & "[91m" & "↓ " & Current_Price'Image & " 💥" & ASCII.ESC & "[0m");
               else
                  Ada.Text_IO.Put_Line (ASCII.ESC & "[93m" & "→ " & Current_Price'Image & " 💫" & ASCII.ESC & "[0m");
               end if;
            end;
            
            Next_Update := Next_Update + Update_Interval;
         end select;
      end loop;
   end Price_Feed_Task;

end Server.Tasks.Price_Feed;
