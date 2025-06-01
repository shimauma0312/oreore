with Ada.Calendar;
with Interfaces; use Interfaces;

package Common.Models is
   -- 注文の種類
   type Order_Side is (Buy, Sell);
   
   -- 注文タイプ
   type Order_Type is (Market, Limit);
   
   -- 注文情報
   type Order is record
      Id          : Unsigned_32;     -- 注文ID
      Side        : Order_Side;      -- 売り/買い
      Order_Type  : Order_Type;      -- 注文種別
      Quantity    : Positive;        -- 数量
      Price       : Natural;         -- 価格 (0 = 成行注文)
      Client_Id   : Natural;         -- クライアントID
   end record;
   
   -- 約定情報
   type Trade is record
      Id             : Unsigned_32;  -- 取引ID
      Order_Id_Buy   : Unsigned_32;  -- 買い注文ID
      Order_Id_Sell  : Unsigned_32;  -- 売り注文ID
      Price          : Natural;      -- 約定価格
      Quantity       : Positive;     -- 約定数量
      Timestamp      : Ada.Calendar.Time; -- 約定時刻
   end record;
   
end Common.Models;
