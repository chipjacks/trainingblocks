class AddTypeAndNameToActivities < ActiveRecord::Migration[5.1]
  def change
    add_column :activities, :type_, :string
    add_column :activities, :name, :string
  end
end
