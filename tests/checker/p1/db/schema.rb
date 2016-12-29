ActiveRecord::Schema.define(version: 20161209084156) do
  create_table "orders", force: true do |t|
    t.integer  "user_id",                                                 null: false
    t.string   "number",                      limit: 16,                  null: false
    t.string   "currency",                    limit: 16,  default: "USD", null: false
  end
end

