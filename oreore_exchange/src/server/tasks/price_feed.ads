with Common.Client; use Common.Client;

package Server.Tasks.Price_Feed is
   -- 価格フィードタスク定義
   task Price_Feed_Task is
      -- クライアント登録エントリー
      entry Subscribe (C : Client_Access);
      
      -- 現在価格取得エントリー
      entry Get_Current_Price (Price : out Natural);
   end Price_Feed_Task;
end Server.Tasks.Price_Feed;
