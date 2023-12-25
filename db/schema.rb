# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# This file is the source Rails uses to define your schema when running `bin/rails
# db:schema:load`. When creating a new database, `bin/rails db:schema:load` tends to
# be faster and is potentially less error prone than running all of your
# migrations from scratch. Old migrations may fail to apply correctly if those
# migrations use external dependencies or application code.
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema[7.0].define(version: 2023_06_19_003224) do
  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "activities", id: :string, force: :cascade do |t|
    t.date "date"
    t.integer "order"
    t.text "description"
    t.json "data"
    t.bigint "user_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "import_id"
    t.index ["import_id"], name: "index_activities_on_import_id"
    t.index ["user_id"], name: "index_activities_on_user_id"
  end

  create_table "imports", id: :string, force: :cascade do |t|
    t.string "source"
    t.json "data"
    t.bigint "user_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["user_id"], name: "index_imports_on_user_id"
  end

  create_table "settings", force: :cascade do |t|
    t.bigint "user_id"
    t.json "paces"
    t.string "race_distance"
    t.integer "race_duration"
    t.integer "level"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "show_time", default: false
    t.index ["user_id"], name: "index_settings_on_user_id", unique: true
  end

  create_table "users", force: :cascade do |t|
    t.string "email"
    t.string "encrypted_password", default: "", null: false
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at", precision: nil
    t.datetime "remember_created_at", precision: nil
    t.integer "sign_in_count", default: 0, null: false
    t.datetime "current_sign_in_at", precision: nil
    t.datetime "last_sign_in_at", precision: nil
    t.inet "current_sign_in_ip"
    t.inet "last_sign_in_ip"
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
    t.string "provider"
    t.string "uid"
    t.string "auth_token"
    t.string "confirmation_token"
    t.datetime "confirmed_at", precision: nil
    t.datetime "confirmation_sent_at", precision: nil
    t.string "unconfirmed_email"
    t.index ["confirmation_token"], name: "index_users_on_confirmation_token", unique: true
    t.index ["email"], name: "index_users_on_email", unique: true
    t.index ["reset_password_token"], name: "index_users_on_reset_password_token", unique: true
  end

  add_foreign_key "activities", "users"
  add_foreign_key "imports", "users"
  add_foreign_key "settings", "users"
end
