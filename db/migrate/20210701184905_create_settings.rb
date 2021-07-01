class CreateSettings < ActiveRecord::Migration[6.1]
  def change
    create_table :settings do |t|
      t.belongs_to :user, index: { unique: true }, foreign_key: true
      t.json :paces

      t.timestamps
    end
  end
end
