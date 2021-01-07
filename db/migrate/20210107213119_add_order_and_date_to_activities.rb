class AddOrderAndDateToActivities < ActiveRecord::Migration[6.1]
  def change
    add_column :activities, :order, :int
    add_column :activities, :date, :date
    remove_column :users, :entries, :json
  end
end
