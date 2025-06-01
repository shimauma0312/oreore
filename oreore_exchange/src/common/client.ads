with Ada.Containers.Vectors;
with Common.Models; use Common.Models;

package Common.Client is
   -- クライアント参照型
   type Client_Interface is limited interface;
   
   procedure Price_Update 
     (Self : in out Client_Interface;
      New_Price : Natural) is abstract;
   
   procedure Trade_Notification
     (Self : in out Client_Interface;
      Trade_Info : Trade) is abstract;
   
   type Client_Access is access all Client_Interface'Class;
   
   -- クライアントリスト型
   package Client_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Client_Access);
   
   subtype Client_List is Client_Vectors.Vector;
   
end Common.Client;
