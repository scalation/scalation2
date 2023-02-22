
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Jun 17 11:19:14 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Database: Purchase Order Database
 *
 *  @see     Principles of Database Management
 *           The Practical Guide to Storing, Managing and Analyzing Big and Small Data
 *           https://www.pdbmbook.com/
 *           https://www.pdbmbook.com/lecturers
 */

package scalation
package database
package table

import Table._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PurchaseOrderDB` main function uses the `Table` class for simple database application.
 *  Note: purchase_order is abreviated to purchase.
 *  @see https://www.pdbmbook.com/lecturers/slides/access -  Chapter%20SQL.pdf
 *  > runMain scalation.database.table.PurchaseOrderDB
 */
@main def PurchaseOrderDB (): Unit =

    val supplier = Table ("supplier", "supNr, supName, supAddress, supCity, supStatus", "I, S, S, S, I", "supNr")

    val product = Table ("product", "prodNr, prodName, prodtype, availableQuantity", "I, S, S, I", "prodNr")

    val supplies = Table ("supplies", "supNr, prodNr, purchasePrice, delivPeriod", "I, I, D, I", "supNr, prodNr")

    val purchase = Table ("purchase", "poNr, poDate, supNr", "I, S, I", "poNr")

    val po_line = Table ("po_line", "poNr, prodNr, quantity", "I, I, I", "poNr, prodNr")

    supplier.add (21, "Dellwines",       "240, Av. of Americas", "New York",      20)
            .add (32, "Best Wines",      "660, Market Street",   "San Francisco", 90)
            .add (37, "Ad Fundum",       "82, Wacker Drive",     "Chicago",       95)
            .add (52, "Spirits & Co.",   "928, Strip",           "Las Vegas",      0)
            .add (68, "The Wine Depot",  "132, Montgomery St.",  "San Francisco", 10)
            .add (69, "Vinos del Mindo", "4, Collins Avenus",    "Miami",         92)
            .show ()

    product.add (119, "Chateau Miraval, 2015",     "rose",      126)
           .add (154, "Chateau Haut Brion, 2008",  "red",       111) 
           .add (178, "Meerdael Chardonnay, 2014", "sprakling", 101)
           .add (199, "Jacques Selosse, 2012",     "white",     117)
           .add (212, "Billecart-Salmon, 2014",    "white",     132)
           .add (300, "Chateau Rontets, 2013",     "sprakling", 143)
           .add (494, "Veuve-Cliquot, Brut, 2012", "white",     105)
           .add (632, "Meneghetti, Chard., 2010",  "sprakling", 107)
           .show ()

    supplies.add (21, 178, 18.99, 3)
            .add (21, 289, 17.99, 1)
            .add (21, 327, 56.00, 6)
            .add (37, 178, 16.99, 4)
            .add (37, 185, 32.99, 3)
            .add (37, 468, 14.00, 1)
            .add (37, 795, 20.99, 3)
            .show ()

    purchase.add (1511, "2015-03-24", 37)
            .add (1512, "2015-04-01", 94)
            .add (1511, "2015-03-24", 37)
            .add (1513, "2015-04-11", 37)
            .add (1523, "2015-04-19", 37)
            .add (1577, "2015-05-10", 37)
            .add (1594, "2015-05-13", 37)
            .show ()

    po_line.add (1511, 212, 2)
           .add (1511, 345, 4)
           .add (1511, 212, 2)
           .add (1512, 178, 3)
           .add (1513, 668, 7)
           .add (1514, 185, 2)
           .add (1514, 900, 2)
           .add (1523, 900, 3)
           .add (1538, 178, 6)
           .add (1538, 212, 15)
           .add (1560, 900, 9)
           .add (1577, 212, 6)
           .add (1577, 668, 9)
           .show ()

    supplier.save ()
    product.save ()
    supplies.save ()
    purchase.save ()
    po_line.save ()

    // 5(a) List the names of suppliers who supply the product named 'pname1'.  Use 'Meerdael Chardonnay, 2014' for 'pname1'
 
    val sup = π ("supName")(σ ("prodName == 'Meerdael Chardonnay, 2014'")(supplier ⋈ supplies ⋈ product))
    sup.show ()

    val sup2 = (supplier ⋈ supplies ⋈ product).σ ("prodName == 'Meerdael Chardonnay, 2014'").π ("supName")
    sup2.show ()

    // 5(b) List the names of suppliers who supply the product named 'pname1' at the lowest price.

    val sup_pr = (supplies ⋈ product).σ ("prodName == 'Meerdael Chardonnay, 2014'") //.π ("supNr, purchasePrice")
    sup_pr.show ()
    val lowest = ((sup_pr.π ("supNr") - sup_pr.⋈ ("purchasePrice > purchasePrice", sup_pr).π ("supNr")) ⋈ supplier).π ("supName")
    lowest.show ()

    // Write RA Queries Here (option also write SQL)

    // 7.1E Write an RA/SQL query that retrieves each supplier who can deliver product 468 within one or two
    //      days, accompanied by the price of the product and the delivery period.

    // 7.3E Write an RA/SQL query that retrieves all pairs of suppliers who supply the same product, along with
    //      their product purchase price if applicable.

    // 7.5E Write an RA/SQL query that retrieves the supplier number, name and status of each supplier who
    //      has a status higher than supplier number 21.
    
    // 7.7E Write an RA/SQL query that retrieves all cities with more than one supplier.

    // 7.15E Write an RA/SQL query that retrieves all purchase order numbers of purchase orders that contain
    //       either sparkling or red wine.

    // 7.17E Write an RA/SQL query that retrieves the name of the product with highest available quantity.

end PurchaseOrderDB

