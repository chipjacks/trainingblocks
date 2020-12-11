class CreateActivities < ActiveRecord::Migration[6.1]
  def change
    create_table :activities, id: false do |t|
      t.string :id, primary_key:  true
      t.text :description
      t.json :data
      t.references :user, null: false, foreign_key: true

      t.timestamps
    end

    add_column :users, :entries, :json
  end
end
