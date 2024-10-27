private with Ada.Containers.Indefinite_Holders;

------------
-- Daseot --
------------

generic
   type Scalar (<>) is private;
package Daseot is

   --  This package uses thread-unsafe by-copy semantics

   type Tree is tagged private;

   function Is_Empty (This : Tree) return Boolean;

   function Is_Populated (This : Tree) return Boolean is (not This.Is_Empty);

   type Kinds is (Atom_Kind, Dict_Kind, List_Kind);

   type Node is abstract tagged private;

   function Kind (This : Node'Class) return Kinds;

   function Root (This : Tree) return Node'Class
     with Pre => This.Is_Populated;

   function Atom (This : Node'Class) return Scalar
     with Pre => This.Kind = Atom_Kind;

   ------------
   --  Refs  --
   ------------

   --  Edition of trees/nodes is done through a Ref, which should never be
   --  stored.

   type Ref (<>) is tagged limited private;

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

   --  function New_Dict return Dict;
   --
   --  function Is_Empty (This : Dict) return Boolean;
   --
--  function Is_Populated (This : Dict) return Boolean is (not This.Is_Empty);
   --
   --  procedure Set (This : in out Dict; Key : Keys; Val : Scalar);

   -------------
   --  Lists  --
   -------------

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

   package Scalar_Holders is new Ada.Containers.Indefinite_Holders (Scalar);

   type Mutable_Node (Kind : Kinds := Atom_Kind) is record
      case Kind is
         when Atom_Kind =>
            Value : Scalar_Holders.Holder;
         when Dict_Kind =>
            Dict  : Placeholder;
         when List_Kind =>
            List  : Placeholder;
      end case;
   end record;

   type Real_Node is new Node with record
      Data : Mutable_Node;
   end record;

   type Atom_Node is new Real_Node with null record;

   type Dict_Node is new Real_Node with null record;

   type List_Node is new Real_Node with null record;

   type Ref (Ptr : access Node'Class) is tagged limited null record
     with Type_Invariant => Ref.Ptr.all in Root_Node | Real_Node;

   -------------
   --  IMPLS  --
   -------------

   ----------
   -- Atom --
   ----------

   function Atom (This : Node'Class) return Scalar
   is (Real_Node (This).Data.Value.Element);

   ----------
   -- Kind --
   ----------

   function Kind (This : Node'Class) return Kinds
   is (if This in Atom_Node then Atom_Kind
       elsif This in Dict_Node then Dict_Kind
       elsif This in List_Node then List_Kind
       else raise Program_Error);

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

   ----------
   -- Root --
   ----------

   function Root (This : Tree) return Node'Class
   is (if This.Is_Populated
       then This.Root.Root.Element
       else raise Program_Error with "tree is empty");

   ----------
   -- Root --
   ----------

   function Root (This : aliased in out Tree'Class) return Ref
   is (Ref'(Ptr => This.Root'Access));

end Daseot;
