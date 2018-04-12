class Activity < ApplicationRecord
  belongs_to :user
  validates :type_, :start_date, :duration, presence: true
end
