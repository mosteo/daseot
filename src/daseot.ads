private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Ordered_Maps;

------------
-- Daseot --
------------

------------
-- Daseot --
------------

generic
   type Scalar (<>) is private;
   with function Image (Value : Scalar) return String is <>;
   --  One line UTF-8 encoded
package Daseot is

   --  This package uses thread-unsafe by-copy semantics

   -------------
   --  Trees  --
   -------------

   type Tree is tagged private;

   function Is_Empty (This : Tree) return Boolean;

   function Is_Populated (This : Tree) return Boolean is (not This.Is_Empty);

   type Kinds is (Atom_Kind, Dict_Kind, List_Kind);

   function Image (This : Tree) return String;
   --  Multi-line UTF-8-encoded

   -------------
   --  Nodes  --
   -------------

   type Node is abstract tagged private;

   function Kind (This : Node'Class) return Kinds;

   function Atom (This : Node'Class) return Scalar
     with Pre => This.Kind = Atom_Kind;

   ------------
   --  Refs  --
   ------------

   --  Edition of trees/nodes is done through a Ref, which should never be
   --  stored.

   type Ref (Element : access Node'Class) is tagged limited private with
     Implicit_Dereference => Element,
     Iterable =>
       (First       => First,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Element);

   function Edit (This : aliased Node'Class) return Ref;

   procedure Set (This : Ref; Value : Node'Class);

   procedure Set (This : Ref; Value : Scalar);

   function Root (This : aliased in out Tree'Class) return Ref;

   -------------
   --  Atoms  --
   -------------

   type Atom_Node is new Node with private;

   function New_Atom (Value : Scalar) return Atom_Node;

   --  function Value (This : Atom) return Scalar;
   --
   --  function Value (This : Node'Class) return Scalar
   --    with Pre => This.Kind = Atom_Node;

   -------------
   --  Dicts  --
   -------------

   type Dict_Node is new Node with private;

   subtype Keys is String;

   function New_Dict return Dict_Node;

   function Is_Empty (This : Dict_Node) return Boolean;

   function Is_Populated (This : Dict_Node) return Boolean
   is (not This.Is_Empty);

   procedure Set (This : in out Dict_Node; Key : Keys; Val : Node'Class);

   function Set (This : aliased Dict_Node'Class;
                 Key  : Keys;
                 Val  : Node'Class)
                 return Ref;
   --  Return the updated Dict_Node, for chaining

   -------------
   --  Lists  --
   -------------

   -----------------
   --  Internals  --
   -----------------

   --  These are exposed due to Ada technicalities but are not needed by
   --  clients.

   type Mutable_Node (<>) is private;

   type Cursor (<>) is private;

   function First (This : Ref) return Cursor;

   function Next (This : Ref; C : Cursor) return Cursor;

   function Has_Element (This : Ref; C : Cursor) return Boolean;

   function Element (This : Ref; C : Cursor) return Ref;

private

   type Node is abstract tagged null record;

   package Node_Holders is new Ada.Containers.Indefinite_Holders (Node'Class);

   subtype Placeholder is Node_Holders.Holder; -- TODO: remove

   type Empty is new Node with null record;

   Empty_Node : constant Empty := (null record);

   type Root_Node is new Node with record
      Root : Node_Holders.Holder := Node_Holders.To_Holder (Empty_Node);
   end record;

   --  We use a doubly nested root so we can always return a reference to the
   --  root node.

   type Tree is tagged record
      Root : aliased Root_Node;
   end record
     with Type_Invariant => not Root.Root.Is_Empty;

   package Node_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Keys, Node'Class);

   package Scalar_Holders is new Ada.Containers.Indefinite_Holders (Scalar);

   function Assert_Mutable_Contents (This : Mutable_Node) return Boolean;

   type Mutable_Node (Kind : Kinds := Atom_Kind) is record
      case Kind is
         when Atom_Kind =>
            Value : Scalar_Holders.Holder;
         when Dict_Kind =>
            Dict  : Node_Maps.Map;
         when List_Kind =>
            List  : Placeholder;
      end case;
   end record
     with Type_Invariant => Assert_Mutable_Contents (Mutable_Node);

   type Real_Node is new Node with record
      Data : Mutable_Node;
   end record;

   type Cursor (Kind : Kinds) is record
      case Kind is
         when Atom_Kind =>
            Visited     : Boolean := False;
         when Dict_Kind =>
            Dict_Cursor : Node_Maps.Cursor;
         when List_Kind =>
            List_Cursor : Placeholder;
      end case;
   end record;

   type Atom_Node is new Real_Node with null record;

   type Dict_Node is new Real_Node with null record;

   type List_Node is new Real_Node with null record;

   function Check_Ref_Node (This : Node'Class) return Boolean
   is (This in Root_Node | Real_Node'Class);

   type Ref (Element : access Node'Class) is tagged limited null record
     with Type_Invariant => Check_Ref_Node (Ref.Element.all);

   -------------
   --  IMPLS  --
   -------------

   -----------------------------
   -- Assert_Mutable_Contents --
   -----------------------------

   function Assert_Mutable_Contents (This : Mutable_Node) return Boolean
   is (case This.Kind is
          when Atom_Kind =>
             not This.Value.Is_Empty,
          when Dict_Kind =>
             (for all E of This.Dict => E in Real_Node'Class),
          when others => True);

   ----------
   -- Atom --
   ----------

   function Atom (This : Node'Class) return Scalar
   is (Real_Node (This).Data.Value.Element);

   ----------
   -- Edit --
   ----------

   function Edit (This : aliased Node'Class) return Ref
   is (Element => This'Unrestricted_Access);

   -------------
   -- Element --
   -------------

   function Element (This : Ref; C : Cursor) return Ref
   is (raise Program_Error);

   -----------
   -- First --
   -----------

   function First (This : Ref) return Cursor
   is (case This.Element.Kind is
          when Atom_Kind => (Kind => Atom_Kind, others => <>),
          when Dict_Kind => raise Program_Error,
          when List_Kind => raise Program_Error
      );

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Ref; C : Cursor) return Boolean
   is (raise Program_Error);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Dict_Node) return Boolean
   is (This.Data.Dict.Is_Empty);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Tree) return Boolean
   is (This.Root.Root.Constant_Reference in Empty);

   --------------
   -- New_Atom --
   --------------

   function New_Atom (Value : Scalar) return Atom_Node
   is (Data => (Kind  => Atom_Kind,
                Value => Scalar_Holders.To_Holder (Value)));

   --------------
   -- New_Dict --
   --------------

   function New_Dict return Dict_Node
   is (Data => (Kind   => Dict_Kind,
                others => <>));

   ----------
   -- Next --
   ----------

   function Next (This : Ref; C : Cursor) return Cursor
   is (raise Program_Error);

   ----------
   -- Root --
   ----------

   function Root (This : aliased in out Tree'Class) return Ref
   is (Ref'(Element => This.Root'Access));

end Daseot;
