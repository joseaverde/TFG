with Ada.Containers.Indefinite_Hashed_Maps, Ada.Strings.Hash;

package String_Maps is

   package String_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   subtype Map is String_Maps.Map;

end String_Maps;
