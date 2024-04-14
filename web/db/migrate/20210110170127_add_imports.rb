class AddImports < ActiveRecord::Migration[6.1]
  def change
    create_table :imports, id: false do |t|
      t.string :id, primary_key: true
      t.string :source
      t.json :data

      t.references :user, null: false, foreign_key: true

      t.timestamps
    end

    add_reference :activities, :import, type: :string
  end
end
