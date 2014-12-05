package controllers

import play.api._
import play.api.db.DB
import play.api.mvc._
import play.api.Play.current
import play.api.libs.json._

object Application extends Controller
{
  def index = Action
  {
    Ok(views.html.index("Your new application is not ready."))
  }

  def hello(name: String) = Action{
    Ok(views.html.hello(name));
  }

  def db_connect = Action {
    var outString = "Number is "
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("SELECT first_name as testkey from patrons")
      while (rs.next()) {
        outString += rs.getString("testkey")
      }
    } finally {
      conn.close()
    }
    Ok(outString)
  }

  def get_available_item() = Action {
    var available_item = ""
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement()
      val rs = stmt.executeQuery("select inventory.inventory_id, items.item_name, items.price, item_types.type_desc " +
        "from inventory left outer join (select loaned_items.inventory_id from loaned_items " +
        "left outer join returned_items on loaned_items.loan_id = returned_items.loan_id " +
        "where returned_items.loan_id is null) as unavailable on inventory.inventory_id = unavailable.inventory_id " +
        "inner join items on items.item_id = inventory.item_id inner join item_types on items.item_type_id = item_types.item_type_id where unavailable.inventory_id is null")
      while(rs.next()){
        val json_available: JsValue = JsObject(Seq(
          rs.getString("inventory_id") -> JsObject(Seq(
            "name" -> JsString(rs.getString("item_name")),
            "price" -> JsString(rs.getString("price")),
            "type" -> JsString(rs.getString("type_desc"))
          ))
        ))

        val jsStr = json_available.toString()
        jsStr.substring(1, jsStr.length-1)
        available_item += jsStr.toString().substring(1, jsStr.length-1) + ","
      }

      available_item = "{" + available_item.substring(0, available_item.length-1) + "}"
    } finally {
      conn.close()
    }
    Ok(available_item)
  }

  def get_unavailable_item() = Action {
    var unavailable_item = ""
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement()
      val rs = stmt.executeQuery("select inventory.inventory_id, items.item_name, items.price, item_types.type_desc " +
        "from loaned_items left outer join returned_items on loaned_items.loan_id = returned_items.loan_id " +
        "inner join inventory on inventory.inventory_id = loaned_items.inventory_id " +
        "inner join items on items.item_id = inventory.item_id " +
        "inner join item_types on items.item_type_id = item_types.item_type_id where returned_items.loan_id is null")
      while(rs.next()){
        val json_unavailable: JsValue = JsObject(Seq(
          rs.getString("inventory_id") -> JsObject(Seq(
            "name" -> JsString(rs.getString("item_name")),
            "price" -> JsString(rs.getString("price")),
            "type" -> JsString(rs.getString("type_desc"))
          ))
        ))

        val jsStr = json_unavailable.toString()
        jsStr.substring(1, jsStr.length-1)
        unavailable_item += jsStr.toString().substring(1, jsStr.length-1) + ","
      }

      unavailable_item = "{" + unavailable_item.substring(0, unavailable_item.length-1) + "}"
    } finally {
      conn.close()
    }
    Ok(unavailable_item)
  }

  def get_patron(patron_id: Long) = Action {
    var patron_info = ""
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("select first_name, last_name, address, phone_number from patrons where patron_id=" + patron_id)
      while (rs.next()) {
        val json_patron: JsValue = JsObject(Seq(
          "firstName" -> JsString(rs.getString("first_name")),
          "lastName" -> JsString(rs.getString("last_name")),
          "address" -> JsString(rs.getString("address")),
          "phoneNumber" -> JsString(rs.getString("phone_number"))
        ))
        patron_info += json_patron
      }
    } finally {
      conn.close()
    }
    Ok(patron_info)
  }

  def get_patron_by_options(first_name: Option[String],
                            last_name: Option[String],
                            address: Option[String],
                            phone_number: Option[String]) = Action {
    var patrons = ""
    val conn = DB.getConnection()
    val where_clause = create_where_from_opts(first_name, last_name, address, phone_number)
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("select patron_id, first_name, last_name, address, phone_number from patrons " + where_clause)
      while(rs.next())
      {
        val json_patrons: JsValue = JsObject(Seq(
          rs.getString("patron_id") -> JsObject(Seq(
            "firstName" -> JsString(rs.getString("first_name")),
            "lastName" -> JsString(rs.getString("last_name")),
            "address" -> JsString(rs.getString("address")),
            "phoneNumber" -> JsString(rs.getString("phone_number"))
          ))
        ))

        val jsStr = json_patrons.toString()
        jsStr.substring(1, jsStr.length-1)
        patrons += jsStr.toString().substring(1, jsStr.length-1) + ","
      }

      patrons = "{" + patrons.substring(0, patrons.length-1) + "}"
    } finally {
      conn.close()
    }
    Ok(patrons)
  }

  def get_inventory_by_opts(inventory_id: Option[Long],
                            item_name: Option[String],
                            price: Option[Long], type_desc: Option[String]) = Action {

    var inventory_list = ""
    var where_clause = " where"
    if (!inventory_id.isEmpty) where_clause += " inventory_id=" + "'" + inventory_id + "'"
    if (!item_name.isEmpty) where_clause += " item_name=" + "'" + item_name + "'"
    if (!price.isEmpty) where_clause += " price=" + "'" +  price + "'"
    if (!type_desc.isEmpty) where_clause += " type_desc=" + "'" +  type_desc + "'"
    if(inventory_id.isEmpty && item_name.isEmpty && price.isEmpty && type_desc.isEmpty) where_clause = ""
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("select * from inventory inner join items on inventory.item_id=items.item_id " +
        "inner join item_types on items.item_type_id=item_types.item_type_id" + where_clause)
      while(rs.next()) {
        val json_inventory: JsValue = JsObject(Seq(
          rs.getString("inventory_id") -> JsObject(Seq(
            "name" -> JsString(rs.getString("item_name")),
            "price" -> JsString(rs.getString("price")),
            "type" -> JsString(rs.getString("type_desc"))
          ))
        ))

        val jsStr = json_inventory.toString()
        jsStr.substring(1, jsStr.length-1)
        inventory_list += jsStr.toString().substring(1, jsStr.length-1) + ","
      }

      inventory_list = "{" + inventory_list.substring(0, inventory_list.length-1) + "}"
    }finally {
      conn.close()
    }
    Ok(inventory_list)
  }

  def add_user_to_db(patron_id: Long,
                     first_name: String,
                     last_name: String,
                     address: String,
                     phone_number: String) = Action {

    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement
      stmt.executeUpdate("insert into patrons (patron_id, first_name, last_name, address, phone_number) values (" +
        patron_id + ", '" +
        first_name + "', '" +
        last_name + "', '" +
        address + "', '" +
        phone_number + "')")
    } finally {
      conn.close()
    }
    Ok("Account Added");
  }

  def create_where_from_opts(first_name: Option[String],
                             last_name: Option[String],
                             address: Option[String],
                             phone_number: Option[String]):  String = {
    var where_clause = "where"

    if (!first_name.isEmpty) where_clause += " first_name=" + "'" + first_name.productElement(0)+ "'"
    if (!last_name.isEmpty) where_clause += " last_name=" + "'" + last_name.productElement(0) + "'"
    if (!address.isEmpty) where_clause += " address=" + "'" +  address.productElement(0) + "'"
    if (!phone_number.isEmpty) where_clause += " phone_number=" + "'" +  phone_number.productElement(0) + "'"

    if (where_clause == "where") return ""
    else return where_clause
  }

  def rent_item(inventory_id: Long, patron_id: Long) = Action {
    val conn = DB.getConnection()
    val timestamp: Long = System.currentTimeMillis / 1000
    try {
      val stmt = conn.createStatement
      stmt.executeUpdate("insert into loaned_items (patron_id, inventory_id, loan_date) values (" + patron_id + ", " + inventory_id + ", " + timestamp.toLong + ")")
    } finally {
      conn.close()
    }
    Ok("Item Rented");
  }

  def return_item(loan_id: Long, patron_id: Long) = Action {
    val conn = DB.getConnection()
    val timestamp: Long = System.currentTimeMillis / 1000
    try {
      val stmt = conn.createStatement
      stmt.executeUpdate("insert into returned_items (patron_id, loan_id, return_date) values (" + patron_id + ", " + loan_id + ", " + timestamp.toLong + ")")
    } finally {
      conn.close()
    }
    Ok("Item Returned")
  }

  def get_rented_items() = Action {
    val conn = DB.getConnection()
    var rented_items = ""
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("select * from loaned_items")
      while(rs.next()){
        val json_inventory: JsValue = JsObject(Seq(
          rs.getString("loanId") -> JsObject(Seq(
            "patronId" -> JsString(rs.getString("patron_id")),
            "loanDate" -> JsString(rs.getString("loan_date")),
            "inventoryId" -> JsString(rs.getString("inventory_id"))
          ))
        ))

        val jsStr = json_inventory.toString()
        jsStr.substring(1, jsStr.length-1)
        rented_items += jsStr.toString().substring(1, jsStr.length-1) + ","
      }

      rented_items = "{" + rented_items.substring(0, rented_items.length-1) + "}"
    } finally {
      conn.close()
    }
    Ok(rented_items)
  }

  def get_returned_items() = Action {
    val conn = DB.getConnection()
    var returned_items = ""
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("select * from returned_items")
      while(rs.next()){
        val json_inventory: JsValue = JsObject(Seq(
          rs.getString("return_id") -> JsObject(Seq(
            "loanId" -> JsString(rs.getString("loan_id")),
            "patronId" -> JsString(rs.getString("patron_id")),
            "returnDate" -> JsString(rs.getString("return_date"))
          ))
        ))

        val jsStr = json_inventory.toString()
        jsStr.substring(1, jsStr.length-1)
        returned_items += jsStr.toString().substring(1, jsStr.length-1) + ","
      }

      returned_items = "{" + returned_items.substring(0, returned_items.length-1) + "}"
    } finally {
      conn.close()
    }
    Ok(returned_items)
  }
}