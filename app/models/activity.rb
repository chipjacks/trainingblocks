class Activity < ApplicationRecord
  belongs_to :user
  attr_accessor :date

  def self.list(user)
    activities = user.activities
    user.entries.map do |e|
      a = activities.find{ |a| a.id == e['id'] }.serializable_hash
      a['date'] = e['date']
      a
    end
  end
end
