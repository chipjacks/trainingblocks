class CreateActivities < ActiveRecord::Migration[5.1]
  def change
    create_table :activities do |t|
      t.references :user, foreign_key: true
      t.datetime :start_date
      t.integer :duration
      t.float :distance
      t.boolean :completed
      t.string :external_id

      t.timestamps
    end
  end
end
