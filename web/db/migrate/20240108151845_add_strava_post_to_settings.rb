class AddStravaPostToSettings < ActiveRecord::Migration[7.0]
  def change
    add_column :settings, :strava_post, :boolean, default: true
  end
end
