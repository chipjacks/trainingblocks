class CreateActivities < ActiveRecord::Migration[6.1]
  def change
    create_table :activities, id: false do |t|
      t.string :id, primary_key: true
      t.date :date
      t.integer :order

      t.text :description
      t.json :data

      t.references :user, null: false, foreign_key: true

      t.timestamps
    end
  end
end
