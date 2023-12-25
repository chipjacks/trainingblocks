class AddShowTimeToSettings < ActiveRecord::Migration[7.0]
  def change
    add_column :settings, :show_time, :boolean, default: false
  end
end
