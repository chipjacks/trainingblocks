class AddStaleToImport < ActiveRecord::Migration[6.1]
  def change
    add_column :imports, :stale, :boolean, default: false
  end
end
