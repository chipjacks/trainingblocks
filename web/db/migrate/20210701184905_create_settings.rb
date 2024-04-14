class CreateSettings < ActiveRecord::Migration[6.1]
  def change
    create_table :settings do |t|
      t.belongs_to :user, index: { unique: true }, foreign_key: true
      t.json :paces
      t.string :race_distance
      t.integer :race_duration
      t.integer :level

      t.timestamps
    end
  end
end
