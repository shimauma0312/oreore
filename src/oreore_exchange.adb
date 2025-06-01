with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Oreore_Exchange is
   -- ASCII アート バナーの表示
   procedure Display_Banner is
   begin
      Ada.Text_IO.Put_Line ("┌─────────────────────────────────────────┐");
      Ada.Text_IO.Put_Line ("│                                         │");
      Ada.Text_IO.Put_Line ("│        オ レ オ レ 証 券 取 引 所        │");
      Ada.Text_IO.Put_Line ("│                                         │");
      Ada.Text_IO.Put_Line ("│         OREORE EXCHANGE v0.1            │");
      Ada.Text_IO.Put_Line ("│                                         │");
      Ada.Text_IO.Put_Line ("└─────────────────────────────────────────┘");
      Ada.Text_IO.New_Line;
   end Display_Banner;
begin
   Display_Banner;
   Ada.Text_IO.Put_Line ("開発中... 取引所サーバーを初期化しています");
end Oreore_Exchange;
