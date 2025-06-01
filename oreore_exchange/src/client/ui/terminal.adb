with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Server.Tasks.Price_Feed;
with Server.Tasks.Order_Book;

package body Client.Terminal is

   task body Terminal_Client_Task is
      -- クライアント情報
      My_Id    : Natural;
      My_Name  : String (1 .. 20) := (others => ' ');
      Name_Len : Natural := 0;
      
      -- 現在価格
      Current_Price : Natural := 0;
      
      -- 注文IDカウンター
      Next_Order_Id : Unsigned_32 := 1;
      
      -- 取引履歴
      Trade_History : Trade_Log_Array;
      History_Index : Natural := 0;
      
      -- 板情報
      Book : Book_Display;
      
      -- イキりメーター（取引回数カウンター）
      Trade_Count : Natural := 0;
      Last_Trade_Time : Ada.Calendar.Time;
      
      -- 画面描画処理
      procedure Draw_Screen is
         use Ada.Text_IO;
         use Ada.Strings.Fixed;
         NL : constant Character := Ada.Characters.Latin_1.LF;
      begin
         -- 画面クリア＆カーソルホーム
         Put (ANSI_CLEAR & ANSI_HOME);
         
         -- ヘッダー表示（宇宙仕様）
         Put_Line (ANSI_BOLD & ANSI_CYAN & "┌──────────────────────────────────────────────────────┐");
         Put_Line ("│                                                      │");
         Put_Line ("│      ★★★ 宇宙仕様 オレオレ証券取引所 ★★★        │");
         Put_Line ("│                                                      │");
         Put_Line ("│        COSMIC OREORE EXCHANGE v0.2 [" & My_Name (1 .. Name_Len) & "]" & 
                  (20 - Name_Len) * " " & "   │");
         Put_Line ("│                                                      │");
         Put_Line ("│      ロケット: 🚀  月: 🌕  火星: 👽  地球: 💰      │");
         Put_Line ("│                                                      │");
         Put_Line ("└──────────────────────────────────────────────────────┘" & ANSI_RESET);
         
         -- 現在価格表示（宇宙仕様）
         Put (ANSI_BOLD & "🌟 現在価格: ");
         if Current_Price > 0 then
            -- 前回の価格と比較して色を変更
            declare
               -- サーバーから最新価格を取得
               Server_Price : Natural;
            begin
               Server.Tasks.Price_Feed.Price_Feed_Task.Get_Current_Price (Server_Price);
               
               if Server_Price > Current_Price then
                  -- 価格上昇: 緑色
                  Put (ANSI_BRIGHT_GREEN & "↑" & Current_Price'Image & " 🚀" & ANSI_RESET & NL & NL);
               elsif Server_Price < Current_Price then
                  -- 価格下落: 赤色
                  Put (ANSI_BRIGHT_RED & "↓" & Current_Price'Image & " 💥" & ANSI_RESET & NL & NL);
               else
                  -- 変化なし: 黄色
                  Put (ANSI_BRIGHT_YELLOW & "→" & Current_Price'Image & " 💫" & ANSI_RESET & NL & NL);
               end if;
               
               -- 価格を更新
               Current_Price := Server_Price;
            end;
         else
            Put (ANSI_YELLOW & "取得中..." & ANSI_RESET & NL & NL);
         end if;
         
         -- 注文入力フォーム
         Put_Line (ANSI_BOLD & "【注文入力】" & ANSI_RESET);
         Put_Line (" 1: 買い指値  2: 買い成行  3: 売り指値  4: 売り成行  0: 終了");
         Put_Line ("");
         
         -- 板情報表示
         Put_Line (ANSI_BOLD & "【板情報】" & ANSI_RESET);
         Put_Line (" " & ANSI_RED & "売り" & ANSI_RESET & "  |  " & 
                  ANSI_GREEN & "買い" & ANSI_RESET);
         Put_Line ("------------------------");
         
         for I in 0 .. Book.Levels - 1 loop
            if Book.Asks (Book.Levels - I - 1) > 0 then
               Put (ANSI_RED & Tail (Book.Asks (Book.Levels - I - 1)'Image, 5) & " " &
                   Tail (Book.Ask_Qtys (Book.Levels - I - 1)'Image, 4) & ANSI_RESET);
            else
               Put ("          ");
            end if;
            
            Put (" | ");
            
            if Book.Bids (I) > 0 then
               Put (ANSI_GREEN & Tail (Book.Bids (I)'Image, 5) & " " &
                   Tail (Book.Bid_Qtys (I)'Image, 4) & ANSI_RESET);
            else
               Put ("          ");
            end if;
            
            New_Line;
         end loop;
         
         -- 取引履歴表示
         Put_Line (NL & ANSI_BOLD & "【約定履歴】" & ANSI_RESET);
         
         for I in 0 .. History_Size - 1 loop
            declare
               Idx : constant Natural := (History_Index - I - 1) mod History_Size;
            begin
               if Trade_History (Idx).Is_Valid then
                  declare
                     T : Trade renames Trade_History (Idx).Trade_Info;
                     Seconds : constant Duration := 
                        Ada.Calendar.Clock - T.Timestamp;
                     Time_Str : constant String :=
                        (if Seconds < 60.0 then
                           Seconds'Image (1 .. 4) & "s ago"
                         else
                           Natural (Seconds / 60.0)'Image & "m ago");
                  begin
                     Put (ANSI_CYAN & Time_Str & ANSI_RESET & " ");
                     Put (ANSI_BOLD & "ID:" & T.Id'Image & ANSI_RESET & " ");
                     
                     Put (ANSI_GREEN & "Price:" & T.Price'Image & ANSI_RESET & " ");
                     Put (ANSI_YELLOW & "Qty:" & T.Quantity'Image & ANSI_RESET);
                     New_Line;
                  end;
               end if;
            end;
         end loop;
         
         -- イキりメーター表示（カラーバーゲージ）
         Put_Line (NL & ANSI_BOLD & "【宇宙仕様イキりメーター】" & ANSI_RESET);
         declare
            Gauge_Width : constant := 50;
            Fill_Width : constant Natural := Natural'Min (Gauge_Width, Trade_Count * 5);
            Empty_Width : constant Natural := Gauge_Width - Fill_Width;
            
            -- 取引数に応じてカラーを変更
            Meter_Color : constant String :=
              (if Trade_Count < 2 then ANSI_BLUE       -- 少ない取引: 青
               elsif Trade_Count < 5 then ANSI_GREEN   -- 普通の取引: 緑
               elsif Trade_Count < 10 then ANSI_YELLOW -- 活発な取引: 黄
               else ANSI_RED);                         -- 超活発: 赤
               
            -- 取引数に応じて表示キャラクターを変更
            Meter_Char : constant String :=
              (if Trade_Count < 2 then "▪"       -- 少ない取引
               elsif Trade_Count < 5 then "■"    -- 普通の取引
               elsif Trade_Count < 10 then "★"   -- 活発な取引
               else "🚀");                        -- 超活発: ロケット
               
            -- ステータステキスト
            Status_Text : constant String :=
              (if Trade_Count < 2 then "地球周回軌道"
               elsif Trade_Count < 5 then "月へ向かう途中"
               elsif Trade_Count < 10 then "火星接近中"
               else "🔥🔥宇宙の彼方へ🔥🔥");
         begin
            Put ("[");
            Put (Meter_Color & (Fill_Width / (if Trade_Count < 10 then 1 else Length (Meter_Char))) * Meter_Char & ANSI_RESET);
            Put (Empty_Width * " ");
            Put ("] " & Trade_Count'Image & " trades - " & Status_Text);
            New_Line;
         end;
         
         -- コマンド入力プロンプト
         Put (NL & "> ");
      end Draw_Screen;
      
      -- 注文送信処理
      procedure Send_Order (Side : Order_Side; Order_Type : Order_Type; 
                           Price : Natural; Quantity : Positive) is
         New_Order : Order := (
            Id         => Next_Order_Id,
            Side       => Side,
            Order_Type => Order_Type,
            Quantity   => Quantity,
            Price      => Price,
            Client_Id  => My_Id
         );
         Result : Natural;
      begin
         -- 注文IDの更新
         Next_Order_Id := Next_Order_Id + 1;
         
         -- 注文送信
         Server.Tasks.Order_Book.Order_Book_Task.Place_Order (New_Order, Result);
         
         -- 結果表示
         if Result = 0 then
            Ada.Text_IO.Put_Line (ANSI_YELLOW & "成行注文が未約定でした" & ANSI_RESET);
         elsif Result = 1 then
            Ada.Text_IO.Put_Line (ANSI_CYAN & "注文が板に追加されました" & ANSI_RESET);
         elsif Result = 2 then
            Ada.Text_IO.Put_Line (ANSI_GREEN & "注文が約定しました" & ANSI_RESET);
         end if;
      end Send_Order;
      
      -- 次の画面更新時刻
      Next_Update : Ada.Calendar.Time;
      
   begin
      -- クライアント情報の初期化
      accept Start (Client_Id : in Natural; Name : in String) do
         My_Id := Client_Id;
         
         -- 名前の設定
         Name_Len := Natural'Min (Name'Length, My_Name'Length);
         My_Name (1 .. Name_Len) := Name (Name'First .. Name'First + Name_Len - 1);
      end Start;
      
      -- 価格フィードに登録
      Server.Tasks.Price_Feed.Price_Feed_Task.Subscribe (Client_Access (Terminal_Client_Task'Unchecked_Access));
      
      -- 注文板に登録
      Server.Tasks.Order_Book.Order_Book_Task.Subscribe (Client_Access (Terminal_Client_Task'Unchecked_Access));
      
      -- 初期画面描画
      Draw_Screen;
      
      -- 初期画面更新時刻設定
      Next_Update := Ada.Calendar.Clock + Screen_Update_Interval;
      
      -- 取引時刻初期化
      Last_Trade_Time := Ada.Calendar.Clock;
      
      -- メインループ
      loop
         select
            -- 価格更新通知の受信
            accept Price_Update (New_Price : Natural) do
               Current_Price := New_Price;
            end Price_Update;
            
         or
            -- 約定通知の受信
            accept Trade_Notification (Trade_Info : Trade) do
               -- 取引履歴に追加
               Trade_History (History_Index) := (Trade_Info, True);
               History_Index := (History_Index + 1) mod History_Size;
               
               -- 自分の注文の場合、イキりメーターを更新
               if Trade_Info.Order_Id_Buy = Next_Order_Id - 1 or
                  Trade_Info.Order_Id_Sell = Next_Order_Id - 1 then
                  Trade_Count := Trade_Count + 1;
                  Last_Trade_Time := Ada.Calendar.Clock;
               end if;
            end Trade_Notification;
            
         or
            -- 定期的な板情報取得と画面更新
            delay until Next_Update;
            
            -- 板情報の取得
            Server.Tasks.Order_Book.Order_Book_Task.Get_Order_Book_Snapshot (
               Book.Bids, Book.Asks, Book.Bid_Qtys, Book.Ask_Qtys, Book.Levels);
            
            -- 画面の再描画
            Draw_Screen;
            
            -- イキりメーターの減衰（30秒間取引がなければリセット）
            if Ada.Calendar.Clock - Last_Trade_Time > 30.0 then
               Trade_Count := Natural'Max (0, Trade_Count - 1);
            end if;
            
            -- 次の更新時刻設定
            Next_Update := Ada.Calendar.Clock + Screen_Update_Interval;
            
         or
            -- ユーザー入力の処理
            declare
               Input : String (1 .. 100);
               Last  : Natural;
               Command : Integer;
            begin
               Ada.Text_IO.Get_Line (Input, Last);
               
               if Last > 0 then
                  begin
                     Ada.Integer_Text_IO.Get (Input (1 .. Last), Command, Last);
                     
                     case Command is
                        when 0 =>
                           -- 終了
                           Ada.Text_IO.Put_Line (ANSI_YELLOW & "終了します..." & ANSI_RESET);
                           exit;
                           
                        when 1 =>
                           -- 買い指値
                           declare
                              Price : Natural;
                              Qty   : Positive;
                           begin
                              Ada.Text_IO.Put (ANSI_CYAN & "価格: " & ANSI_RESET);
                              Ada.Text_IO.Get (Input, Last);
                              Ada.Integer_Text_IO.Get (Input (1 .. Last), Price, Last);
                              
                              Ada.Text_IO.Put (ANSI_CYAN & "数量: " & ANSI_RESET);
                              Ada.Text_IO.Get (Input, Last);
                              Ada.Integer_Text_IO.Get (Input (1 .. Last), Qty, Last);
                              
                              Send_Order (Buy, Limit, Price, Qty);
                           end;
                           
                        when 2 =>
                           -- 買い成行
                           declare
                              Qty : Positive;
                           begin
                              Ada.Text_IO.Put (ANSI_CYAN & "数量: " & ANSI_RESET);
                              Ada.Text_IO.Get (Input, Last);
                              Ada.Integer_Text_IO.Get (Input (1 .. Last), Qty, Last);
                              
                              Send_Order (Buy, Market, 0, Qty);
                           end;
                           
                        when 3 =>
                           -- 売り指値
                           declare
                              Price : Natural;
                              Qty   : Positive;
                           begin
                              Ada.Text_IO.Put (ANSI_CYAN & "価格: " & ANSI_RESET);
                              Ada.Text_IO.Get (Input, Last);
                              Ada.Integer_Text_IO.Get (Input (1 .. Last), Price, Last);
                              
                              Ada.Text_IO.Put (ANSI_CYAN & "数量: " & ANSI_RESET);
                              Ada.Text_IO.Get (Input, Last);
                              Ada.Integer_Text_IO.Get (Input (1 .. Last), Qty, Last);
                              
                              Send_Order (Sell, Limit, Price, Qty);
                           end;
                           
                        when 4 =>
                           -- 売り成行
                           declare
                              Qty : Positive;
                           begin
                              Ada.Text_IO.Put (ANSI_CYAN & "数量: " & ANSI_RESET);
                              Ada.Text_IO.Get (Input, Last);
                              Ada.Integer_Text_IO.Get (Input (1 .. Last), Qty, Last);
                              
                              Send_Order (Sell, Market, 0, Qty);
                           end;
                           
                        when others =>
                           Ada.Text_IO.Put_Line (ANSI_RED & "無効なコマンドです" & ANSI_RESET);
                     end case;
                  exception
                     when others =>
                        Ada.Text_IO.Put_Line (ANSI_RED & "入力エラー" & ANSI_RESET);
                  end;
                  
                  -- 画面再描画
                  Draw_Screen;
               end if;
            end;
         end select;
      end loop;
      
      Ada.Text_IO.Put_Line (ANSI_CLEAR & ANSI_HOME);
   end Terminal_Client_Task;

end Client.Terminal;
