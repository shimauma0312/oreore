with Ada.Text_IO;
with Ada.Command_Line;
with Client.Terminal;

procedure Client_Main is
   -- クライアント情報
   Client_Id : Natural := 1;
   Nick_Name : String := "Anonymous";

begin
   -- コマンドライン引数の処理
   if Ada.Command_Line.Argument_Count >= 2 then
      for I in 1 .. Ada.Command_Line.Argument_Count - 1 loop
         if Ada.Command_Line.Argument(I) = "--id" then
            Client_Id := Natural'Value(Ada.Command_Line.Argument(I + 1));
         elsif Ada.Command_Line.Argument(I) = "--nick" then
            Nick_Name := Ada.Command_Line.Argument(I + 1);
         end if;
      end loop;
   end if;

   -- クライアントタスクの作成と起動
   declare
      Client : Client.Terminal.Terminal_Client_Task;
   begin
      Client.Start(Client_Id, Nick_Name);

      -- メインタスクは単にクライアントタスクが終了するのを待つ
      loop
         delay 1.0;
      end loop;
   end;
end Client_Main;
