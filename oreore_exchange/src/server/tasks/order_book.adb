with Ada.Numerics.Discrete_Random;
with Server.Tasks.Price_Feed;

package body Server.Tasks.Order_Book is

   task body Order_Book_Task is
      -- 買い注文と売り注文のリスト
      Buy_Orders  : Order_List;
      Sell_Orders : Order_List;
      
      -- クライアントリスト
      Clients : Client_List;
      
      -- 注文マッチング処理
      procedure Match_Orders (New_Order : in Order; Matched : out Boolean) is
         Match_Price : Natural;
         Match_Qty   : Positive;
         Counter_Order : Order;
         New_Trade   : Trade;
      begin
         Matched := False;
         
         -- 買い注文の場合、売り注文リストとマッチング
         if New_Order.Side = Buy then
            for I in 0 .. Natural (Sell_Orders.Length) - 1 loop
               Counter_Order := Sell_Orders (I);
               
               -- 価格条件の確認（成行注文または指値が合致）
               if (New_Order.Order_Type = Market) or else 
                  (Counter_Order.Price <= New_Order.Price) then
                  
                  -- 約定価格決定（先に出した注文の価格を優先）
                  Match_Price := Counter_Order.Price;
                  
                  -- 約定数量決定（小さい方の数量）
                  Match_Qty := Positive'Min (New_Order.Quantity, Counter_Order.Quantity);
                  
                  -- 約定情報作成
                  New_Trade := (
                     Id             => Next_Trade_Id,
                     Order_Id_Buy   => New_Order.Id,
                     Order_Id_Sell  => Counter_Order.Id,
                     Price          => Match_Price,
                     Quantity       => Match_Qty,
                     Timestamp      => Ada.Calendar.Clock
                  );
                  
                  Next_Trade_Id := Next_Trade_Id + 1;
                  
                  -- クライアントへ通知
                  for J in 0 .. Natural (Clients.Length) - 1 loop
                     if Clients (J) /= null then
                        Clients (J).Trade_Notification (New_Trade);
                     end if;
                  end loop;
                  
                  -- 対向注文の更新または削除
                  if Counter_Order.Quantity > Match_Qty then
                     -- 部分約定、残りを板に残す
                     Counter_Order.Quantity := Counter_Order.Quantity - Match_Qty;
                     Sell_Orders (I) := Counter_Order;
                  else
                     -- 完全約定、板から削除
                     Sell_Orders.Delete (I);
                  end if;
                  
                  Matched := True;
                  Ada.Text_IO.Put (ASCII.ESC & "[95m" & "🔄 Order matched: " & ASCII.ESC & "[0m");
                  Ada.Text_IO.Put (ASCII.ESC & "[93m" & "Trade ID:" & New_Trade.Id'Image & ASCII.ESC & "[0m" & " ");
                  Ada.Text_IO.Put (ASCII.ESC & "[92m" & "Price:" & Match_Price'Image & ASCII.ESC & "[0m" & " ");
                  Ada.Text_IO.Put_Line (ASCII.ESC & "[96m" & "Qty:" & Match_Qty'Image & ASCII.ESC & "[0m");
                  exit;
               end if;
            end loop;
            
         -- 売り注文の場合、買い注文リストとマッチング
         else
            for I in 0 .. Natural (Buy_Orders.Length) - 1 loop
               Counter_Order := Buy_Orders (I);
               
               -- 価格条件の確認（成行注文または指値が合致）
               if (New_Order.Order_Type = Market) or else 
                  (Counter_Order.Price >= New_Order.Price) then
                  
                  -- 約定価格決定（先に出した注文の価格を優先）
                  Match_Price := Counter_Order.Price;
                  
                  -- 約定数量決定（小さい方の数量）
                  Match_Qty := Positive'Min (New_Order.Quantity, Counter_Order.Quantity);
                  
                  -- 約定情報作成
                  New_Trade := (
                     Id             => Next_Trade_Id,
                     Order_Id_Buy   => Counter_Order.Id,
                     Order_Id_Sell  => New_Order.Id,
                     Price          => Match_Price,
                     Quantity       => Match_Qty,
                     Timestamp      => Ada.Calendar.Clock
                  );
                  
                  Next_Trade_Id := Next_Trade_Id + 1;
                  
                  -- クライアントへ通知
                  for J in 0 .. Natural (Clients.Length) - 1 loop
                     if Clients (J) /= null then
                        Clients (J).Trade_Notification (New_Trade);
                     end if;
                  end loop;
                  
                  -- 対向注文の更新または削除
                  if Counter_Order.Quantity > Match_Qty then
                     -- 部分約定、残りを板に残す
                     Counter_Order.Quantity := Counter_Order.Quantity - Match_Qty;
                     Buy_Orders (I) := Counter_Order;
                  else
                     -- 完全約定、板から削除
                     Buy_Orders.Delete (I);
                  end if;
                  
                  Matched := True;
                  Ada.Text_IO.Put (ASCII.ESC & "[95m" & "🔄 Order matched: " & ASCII.ESC & "[0m");
                  Ada.Text_IO.Put (ASCII.ESC & "[93m" & "Trade ID:" & New_Trade.Id'Image & ASCII.ESC & "[0m" & " ");
                  Ada.Text_IO.Put (ASCII.ESC & "[92m" & "Price:" & Match_Price'Image & ASCII.ESC & "[0m" & " ");
                  Ada.Text_IO.Put_Line (ASCII.ESC & "[96m" & "Qty:" & Match_Qty'Image & ASCII.ESC & "[0m");
                  exit;
               end if;
            end loop;
         end if;
      end Match_Orders;
      
      -- 注文追加処理
      procedure Add_Order_To_Book (O : in Order) is
      begin
         if O.Side = Buy then
            Buy_Orders.Append (O);
            Ada.Text_IO.Put (ASCII.ESC & "[94m" & "📝 Buy order added: " & ASCII.ESC & "[0m");
            Ada.Text_IO.Put (ASCII.ESC & "[93m" & "ID:" & O.Id'Image & ASCII.ESC & "[0m" & " ");
            Ada.Text_IO.Put (ASCII.ESC & "[92m" & "Price:" & O.Price'Image & ASCII.ESC & "[0m" & " ");
            Ada.Text_IO.Put_Line (ASCII.ESC & "[96m" & "Qty:" & O.Quantity'Image & ASCII.ESC & "[0m");
         else
            Sell_Orders.Append (O);
            Ada.Text_IO.Put (ASCII.ESC & "[91m" & "📝 Sell order added: " & ASCII.ESC & "[0m");
            Ada.Text_IO.Put (ASCII.ESC & "[93m" & "ID:" & O.Id'Image & ASCII.ESC & "[0m" & " ");
            Ada.Text_IO.Put (ASCII.ESC & "[92m" & "Price:" & O.Price'Image & ASCII.ESC & "[0m" & " ");
            Ada.Text_IO.Put_Line (ASCII.ESC & "[96m" & "Qty:" & O.Quantity'Image & ASCII.ESC & "[0m");
         end if;
      end Add_Order_To_Book;
      
   begin
      -- メインループ
      loop
         select
            -- 注文受付
            accept Place_Order (O : in Order; Result : out Natural) do
               declare
                  Matched : Boolean := False;
                  Current_Order : Order := O;
               begin
                  -- 成行注文の場合、現在価格を取得
                  if O.Order_Type = Market then
                     declare
                        Current_Price : Natural;
                     begin
                        -- 価格フィードから現在価格を取得
                        Server.Tasks.Price_Feed.Price_Feed_Task.Get_Current_Price (Current_Price);
                        Current_Order.Price := Current_Price;
                     end;
                  end if;
                  
                  -- マッチング処理
                  Match_Orders (Current_Order, Matched);
                  
                  -- 未約定の場合、注文板に追加
                  if not Matched and then O.Order_Type = Limit then
                     Add_Order_To_Book (Current_Order);
                     Result := 1; -- 板に追加
                  elsif Matched then
                     Result := 2; -- 約定成立
                  else
                     Result := 0; -- 成行注文が未約定
                  end if;
               end;
            end Place_Order;
            
         or
            -- クライアント登録
            accept Subscribe (C : Client_Access) do
               Clients.Append (C);
            end Subscribe;
            
         or
            -- 板情報取得
            accept Get_Order_Book_Snapshot (
               Top_Bids   : out Natural_Array;
               Top_Asks   : out Natural_Array;
               Bid_Qtys   : out Positive_Array;
               Ask_Qtys   : out Positive_Array;
               Levels     : out Natural) do
               
               -- デフォルト値を設定
               Top_Bids := (others => 0);
               Top_Asks := (others => 0);
               Bid_Qtys := (others => 1);
               Ask_Qtys := (others => 1);
               
               -- 表示レベル数の計算
               Levels := Natural'Min (Max_Display_Levels, 
                                     Natural'Max (Natural (Buy_Orders.Length), 
                                                 Natural (Sell_Orders.Length)));
               
               -- 買い注文をソート（価格の降順）
               declare
                  type Price_Index_Pair is record
                     Price : Natural;
                     Qty   : Positive;
                  end record;
                  
                  type Price_Array is array (Natural range <>) of Price_Index_Pair;
                  Bids : Price_Array (0 .. Natural (Buy_Orders.Length) - 1);
                  
                  procedure Sort_Bids is
                     Temp : Price_Index_Pair;
                  begin
                     for I in Bids'Range loop
                        for J in Bids'First .. Bids'Last - I loop
                           if J < Bids'Last and then Bids (J).Price < Bids (J + 1).Price then
                              Temp := Bids (J);
                              Bids (J) := Bids (J + 1);
                              Bids (J + 1) := Temp;
                           end if;
                        end loop;
                     end loop;
                  end Sort_Bids;
               begin
                  -- 買い注文をソート用配列に設定
                  for I in 0 .. Natural (Buy_Orders.Length) - 1 loop
                     Bids (I) := (Buy_Orders (I).Price, Buy_Orders (I).Quantity);
                  end loop;
                  
                  -- ソート実行
                  Sort_Bids;
                  
                  -- トップN件を結果配列に設定
                  for I in 0 .. Natural'Min (Levels - 1, Bids'Last) loop
                     Top_Bids (I) := Bids (I).Price;
                     Bid_Qtys (I) := Bids (I).Qty;
                  end loop;
               end;
               
               -- 売り注文をソート（価格の昇順）
               declare
                  type Price_Index_Pair is record
                     Price : Natural;
                     Qty   : Positive;
                  end record;
                  
                  type Price_Array is array (Natural range <>) of Price_Index_Pair;
                  Asks : Price_Array (0 .. Natural (Sell_Orders.Length) - 1);
                  
                  procedure Sort_Asks is
                     Temp : Price_Index_Pair;
                  begin
                     for I in Asks'Range loop
                        for J in Asks'First .. Asks'Last - I loop
                           if J < Asks'Last and then Asks (J).Price > Asks (J + 1).Price then
                              Temp := Asks (J);
                              Asks (J) := Asks (J + 1);
                              Asks (J + 1) := Temp;
                           end if;
                        end loop;
                     end loop;
                  end Sort_Asks;
               begin
                  -- 売り注文をソート用配列に設定
                  for I in 0 .. Natural (Sell_Orders.Length) - 1 loop
                     Asks (I) := (Sell_Orders (I).Price, Sell_Orders (I).Quantity);
                  end loop;
                  
                  -- ソート実行
                  Sort_Asks;
                  
                  -- トップN件を結果配列に設定
                  for I in 0 .. Natural'Min (Levels - 1, Asks'Last) loop
                     Top_Asks (I) := Asks (I).Price;
                     Ask_Qtys (I) := Asks (I).Qty;
                  end loop;
               end;
            end Get_Order_Book_Snapshot;
         end select;
      end loop;
   end Order_Book_Task;

end Server.Tasks.Order_Book;
