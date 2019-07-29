--------------------------------------------------------------------------------
--                                                                            --
--                       X A D A . D I S P A T C H I N G                      --
--                                                                            --
--                                   S P E C                                  --
--                                                                            --
-- @author 2018-19 Jorge Real (jorge@disca.upv.es)                            --
-- @author 2018-19 Sergio Saez (ssaez@disca.upv.es)                           --
--                                                                            --
-- This library is free software: you can redistribute it and/or modify it    --
-- under the terms of the GNU Lesser General Public License as published by   --
-- the Free Software Foundation, either version 3 of the License, or (at your --
-- option) any later version.                                                 --
--                                                                            --
-- This library is distributed in the hope that it will be useful, but        --
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public    --
-- License for more details.                                                  --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public License   --
-- along with this program. If not, see <https://www.gnu.org/licenses/>.      --
--                                                                            --
--------------------------------------------------------------------------------

package XAda.Dispatching is
   pragma Pure (Dispatching);

   Dispatching_Policy_Error : exception;
end XAda.Dispatching;
