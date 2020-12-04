class CreateRuns < ActiveRecord::Migration[6.0]
  def change
    create_table :runs do |t|
      t.integer :duration
      t.string :pace
      t.boolean :completed
      t.references :activity, null: false, foreign_key: true

      t.timestamps
    end
  end
end
